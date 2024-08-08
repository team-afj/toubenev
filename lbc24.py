from __future__ import annotations  # allows class type usage in class decl
from typing import List, Dict
from datetime import date, time, datetime, timedelta
from operator import contains
import math
from ortools.sat.python import cp_model
from data_model import Bénévole, Type_de_quête, Quête

# import import_csv
from import_json import from_file

from_file("data/db.json")

quêtes = sorted(Quête.toutes)
bénévoles = Bénévole.tous.values()


def quêtes_dun_lieu(lieu):
    return filter(lambda q: q.lieu == lieu, quêtes)


id_quête_sérénité = "784fc134-cab5-4797-8be2-7a7a91e57795"

""" Outils """


def time_to_minutes(t: time):
    return t.hour * 60 + t.minute


def diff_minutes(t1: time, t2: time):
    return time_to_minutes(t2) - time_to_minutes(t1)


"""Préparation du modèle et des contraintes"""

model = cp_model.CpModel()

""" On créé une variable par bénévole pour chaque "slot" de chaque quête."""
assignations: Dict[(Bénévole, Quête), cp_model.BoolVarT] = {}
""" Ainsi qu'un intervalle correspondant aux horaires de la quête en minutes """
intervalles: Dict[(Bénévole, Quête), cp_model.IntervalVar] = {}

for b in bénévoles:
    for q in quêtes:
        assignations[(b, q)] = model.new_bool_var(f"shift_b{b}_q{q}")
        # TODO: this is gnééé. Quests that end after midnight are not
        # handled correctly
        d: int = time_to_minutes(q.début.time())
        f: int = time_to_minutes(q.fin.time())
        if f < d:
            f = 23 * 60 + 59
        intervalles[(b, q)] = model.new_optional_interval_var(
            d, f - d, f, assignations[(b, q)], f"interval_quête_{b}_{q}"
        )

""" Tous les slots de toutes les quêtes doivent être peuplés """
for q in quêtes:
    model.add(sum(assignations[(b, q)] for b in bénévoles) == q.nombre_bénévoles)


def member(l, x):
    for e in l:
        if e == x:
            return True
    return False


""" Un même bénévole ne peut pas remplir plusieurs quêtes en même temps """

for q in quêtes:
    en_même_temps = list(q.en_même_temps())
    for q2 in en_même_temps:
        for b in bénévoles:
            if q != q2:
                model.add_at_most_one([assignations[(b, q)], assignations[(b, q2)]])

""" Certaines quêtes sont déjà assignées """
for q in quêtes:
    for b in q.bénévoles:
        model.add(assignations[(b, q)] == 1)

""" Les tâches consécutives d'une scène sont faites par les mêmes bénévoles """

quêtes_liées_des_spectacles: List[List[Quête]] = []
for date, quêtes_du_jour in Quête.par_jour.items():
    quêtes_des_spectacles: Dict[str, List[Quête]] = {}
    for q in quêtes_du_jour:
        if q.spectacle:
            quêtes_du_spectacle = quêtes_des_spectacles.get(q.spectacle.id, [])
            if quêtes_du_spectacle == []:
                quêtes_des_spectacles[q.spectacle.id] = [q]
            else:
                quêtes_du_spectacle.append(q)
    for l in quêtes_des_spectacles.values():
        quêtes_liées_des_spectacles.append(l)


def suivi_quêtes_dun_spectacles(quêtes: List[Quête]):
    min_nb = 99
    min_quête = None
    for q in quêtes:
        if q.nombre_bénévoles < min_nb:
            min_nb = q.nombre_bénévoles
            min_quête = q

    # On s'assure que les quêtes groupées ne se chevauchent pas, cela évite des
    # erreurs incompréhensibles:
    for q in quêtes:
        for q2 in q.en_même_temps():
            for q3 in quêtes:
                if q != q3 and q2 == q3:
                    print("Arg, des quêtes groupées se chevauchent:")
                    print(f"{q2} chevauche {q3}")
                    exit()

    for b in bénévoles:
        # Tout bénévole participant à la quête qui requiert le moins de bénévoles
        # dans le groupe de quêtes doit participer aux autres quêtes du groupe.
        model.add_bool_and(assignations[(b, q)] for q in quêtes).only_enforce_if(
            assignations[(b, min_quête)]
        )


for quêtes_dun_spectacle in quêtes_liées_des_spectacles:
    suivi_quêtes_dun_spectacles(quêtes_dun_spectacle)


""" Certains bénévoles sont indisponibles à certains horaires """
for b in bénévoles:
    for q in quêtes:
        # On vérifie que ce n'est pas une quête forcée:
        if not (contains(q.bénévoles, b)):
            for début_indispo in b.indisponibilités:
                fin_indispo = time((début_indispo.hour + 1) % 24)
                if not (fin_indispo <= q.début.time() or début_indispo >= q.fin.time()):
                    model.add(assignations[(b, q)] == 0)

""" Tout le monde ne peut pas assumer les quêtes sérénité """
for b in bénévoles:
    if not (b.sérénité):
        for q in quêtes:
            if contains(q.types, Type_de_quête.tous[id_quête_sérénité]):
                model.add(assignations[(b, q)] == 0)

""" Les lieux interdits sont interdits """
for b in bénévoles:
    for lieu in b.lieux_interdits:
        for q in quêtes_dun_lieu(lieu):
            model.add(assignations[(b, q)] == 0)


""" Ils se détestent, séparez-les ! """
for b in bénévoles:
    for e in b.binômes_interdits:
        for q in quêtes:
            model.add(assignations[(b, q)] + assignations[(e, q)] <= 1)

""" Chacun a un trou dans son emploi du temps """


début_période_pause = 9 * 60
fin_période_pause = 23 * 60
durée_pause_min = 5 * 60  # 5h en minutes


def c_est_la_pause(b: Bénévole):
    for date, quêtes in Quête.par_jour.items():
        début_pause = model.new_int_var(
            début_période_pause,
            fin_période_pause - durée_pause_min,
            f"début_pause_{b}_{date}",
        )
        fin_pause = model.new_int_var(
            début_période_pause + durée_pause_min,
            fin_période_pause,
            f"fin_pause_{b}_{date}",
        )
        size = model.new_int_var(durée_pause_min, 24 * 60, f"size_pause_{b}_{date}")
        interval_pause = model.new_interval_var(
            début_pause,
            size,
            fin_pause,
            f"interval_pause_{b}_{date}",
        )
        # La pause est suffisamment longue:
        model.add(size >= durée_pause_min)
        # Le bénévole n'a aucune quête pendant sa pause:
        overlaps = [interval_pause]
        for q in quêtes:
            overlaps.append(intervalles[(b, q)])
        model.add_no_overlap(overlaps)


for b in bénévoles:
    c_est_la_pause(b)


""" Calcul de la qualité d'une réponse """

""" Contrôle du temps de travail """

# Le temps de travail quotidien maximal de chaque bénévole est stocké, en
# heures, dans le champ `heures_théoriques`. On les converti systématiquement en
# minutes

temps_de_travail_disponible_quotidien = (
    # TODO: this might be different everyday
    60
    * sum(b.heures_théoriques for b in bénévoles)
)

# On va normaliser le temps de travail (merci Malou), on choisit le ppcm des
# heures théoriques des bénévoles comme cible pour s'assurer que le coefficient
# multiplicateur de normalisation soit toujours entier. On ajoute aussi la somme
# de ces heures théoriques pour pouvoir normaliser l'écart total sur une
# journée.
ppcm_heures_théoriques = math.lcm(
    temps_de_travail_disponible_quotidien,
    *[60 * b.heures_théoriques for b in bénévoles],
)


# coef_de(b) renvoie le coefficient de normalisation pour le bénévole b
def coef_de(b: Bénévole):
    # t / heures_théoriques = x / ppcm_heures_theoriques)
    return int(ppcm_heures_théoriques / (b.heures_théoriques * 60))


# Temps de travail d'un bénévole sur un ensemble de quêtes
def temps_bev(b, quêtes, assignations):
    # assignations[(b, q)] pour un bénévole b et une quête q vaut 0 ou 1 et
    # indique si le bénévole a été assigné à cette quête.
    return sum(q.durée_minutes() * assignations[(b, q)] for q in quêtes)


# Temps de travail, par jour, d'un bénévole
def temps_total_bénévole(b, assignations):
    # On renvoie un dictionnaire date -> temps de travail
    return {
        date: temps_bev(b, quêtes, assignations)
        for date, quêtes in Quête.par_jour.items()
    }


def temps_total_quêtes(quêtes: List[Quête]):
    return sum(q.durée_minutes() * q.nombre_bénévoles for q in quêtes)


# Moyenne normalisée : sur une journée, comme toutes les quêtes doivent être
# assignée, la somme des écarts de chaque bénévole par rapport à leur temps de
# travail prévu est constante. Donc (?) la moyenne des écarts est indépendante
# de l'assignation des bénévoles et est égale à l'écart entre la force totale de
# travail disponible et la durée effective des quêtes.
# On renvoie un dictionnaire date -> "moyenne"
moyenne_tdc_norm = {
    date: (
        int(
            (temps_total_quêtes(quêtes) - temps_de_travail_disponible_quotidien)
            * (ppcm_heures_théoriques / temps_de_travail_disponible_quotidien)
            / len(bénévoles)
        )
    )
    for date, quêtes in Quête.par_jour.items()
}


# Calcule la valeur absolue via une variable et une contrainte supplémentaires
def abs_var(id, value: cp_model.LinearExprT):
    var = model.NewIntVar(0, ppcm_heures_théoriques, f"v_abs_{id}")
    model.AddAbsEquality(var, value)
    return var


# Écart de l'écart du temps de travail d'un bénévole par rapport à la moyenne
# Renvoie un dictionnaire indexé par les jours
def diff_temps(b, assignations, coef=1):
    return {
        date: (((b.heures_théoriques * 60 - tdt) * coef) - moyenne_tdc_norm[date])
        for date, tdt in temps_total_bénévole(b, assignations).items()
    }


def squared_var(id, value):
    var = model.NewIntVar(0, pow(ppcm_heures_théoriques, 2), f"v_pow_{id}")
    var_diff = model.NewIntVar(
        -ppcm_heures_théoriques, ppcm_heures_théoriques, f"v_diff_{id}"
    )
    model.add(var_diff == value)
    model.add_multiplication_equality(var, [var_diff, var_diff])
    return var


# Calcule la somme pour chaque jour de la valeur absolue de écarts à la moyenne
# des écarts du bénévole `b`
def écarts_du_bénévole(b):
    diff_par_jour = diff_temps(b, assignations, coef=coef_de(b))
    return sum(
        # abs_var(f"diff_{date}_béné_{b}", diff)
        squared_var(f"diff_{date}_béné_{b}", diff)
        for date, diff in diff_par_jour.items()
    )


""" Punition des excès """
# On filtre les écarts de temps de travail positif pour leur attribuer un poids
# plus fort. Dans l'idéal, s'il y a suffisamment de main d'oeuvre, personne ne
# devrait travailler plus que prévu.


def filter_positive(value, name):
    v = model.new_int_var(0, ppcm_heures_théoriques, name)
    model.add_max_equality(v, [0, value])
    return v


def excès_de_travail(b):
    diff_par_jour = diff_temps(b, assignations, coef=coef_de(b))
    return sum(
        filter_positive(diff, f"excès_{date}_{b}")
        for date, diff in diff_par_jour.items()
    )


""" Pondération des préférences des bénévoles """


def appréciation_dune_quête(bénévole: Bénévole, quête: Quête):
    # On découpe la quête par blocs de 15 minutes
    acc = quête.début
    somme_prefs = 0
    while acc < quête.fin:
        time = acc.time()
        for pref_t, p in bénévole.pref_horaires.items():
            if time.hour == pref_t.hour:
                somme_prefs += p * 2
                break
        acc = min(acc + timedelta(minutes=15), quête.fin)
    return somme_prefs


def appréciation_du_planning(bénévole: Bénévole, quêtes: List[Quête]):
    return sum(
        assignations[(bénévole, q)] * (appréciation_dune_quête(bénévole, q))
        for q in quêtes
    )


""" Distance entre la première et la dernière quête """


def amplitude_horaire(b: Bénévole, quêtes: List[Quête]):
    début = model.new_int_var(0, 60 * 24, f"début_journée_{b}")
    fin = model.new_int_var(0, 60 * 24, f"fin_journée_{b}")
    model.add_min_equality(
        début, map(lambda q: intervalles[(b, q)].start_expr(), quêtes)
    )
    model.add_max_equality(fin, map(lambda q: intervalles[(b, q)].end_expr(), quêtes))
    return fin - début


def amplitudes(b: Bénévole):
    return sum(
        amplitude_horaire(b, quêtes) - (b.heures_théoriques * 60)
        for quêtes in Quête.par_jour.values()
    )


""" Formule finale """

model.minimize(
    sum(
        # Idéalement, personne ne doit trop travailler. Sauf Popi bien sûr
        1000 * excès_de_travail(b)
        + 100 * écarts_du_bénévole(b)
        - 10 * appréciation_du_planning(b, quêtes)
        + 1 * amplitudes(b)
        for b in bénévoles
    )
)


""" Solution printer """


def smile_of_appréciation(app):
    smile = "🙂"
    if app >= 1:
        smile = "🤗"
    if app < 0:
        smile = "😰"
    if app < -1:
        smile = "😭"
    return smile


class VarArraySolutionPrinter(cp_model.CpSolverSolutionCallback):
    """Print intermediate solutions."""

    def __init__(self, assignations):
        cp_model.CpSolverSolutionCallback.__init__(self)
        self._assignations = assignations
        self._solution_count = 0

    def on_solution_callback(self) -> None:
        self._solution_count += 1

        assignations_val = {
            key: self.value(val) for key, val in self._assignations.items()
        }

        smiles = {}
        for q in quêtes:
            for b in bénévoles:
                if assignations_val[(b, q)] == 1:
                    app = appréciation_dune_quête(b, q)
                    smile = smile_of_appréciation(app)
                    smile_count = smiles.get(smile, 0)
                    smiles[smile] = smile_count + 1
        total_smiles = sum(smiles.values())
        smile_kinds = sorted(smiles.keys())
        # x / total_smile = y / 10
        smile_line = ""
        for smile in smile_kinds:
            n = smiles[smile]
            n100 = (n * 100) / total_smiles
            smile_line = f"{smile_line}[{smile}{n100:=04.1f}%]"
        smile_line = f"{smile_line}"

        écarts_line = "["
        écarts = {}
        nombre_bénévoles = len(bénévoles)
        for b in bénévoles:
            diff_par_jour_ = diff_temps(b, assignations_val)
            for d, écart in diff_par_jour_.items():
                (min_, somme, max_) = écarts.get(d, (0, 0, 0))
                écarts[d] = (
                    min(min_, écart),
                    (somme + abs(écart)),
                    max(max_, écart),
                )

        for _, (min_, écart_total, max_) in écarts.items():
            écart_type = math.sqrt(écart_total / nombre_bénévoles)
            écarts_line = f"{écarts_line} {écart_type:=.1f} [{min_};{max_}]"
        écarts_line = f"{écarts_line}]"

        print(f"Solution {self._solution_count:0=3d}:\n\t{écarts_line}\n\t{smile_line}")

    @property
    def solution_count(self) -> int:
        return self.__solution_count


# Enumerate all solutions.
solver = cp_model.CpSolver()
solution_printer = VarArraySolutionPrinter(assignations)
solver.parameters.log_search_progress = True
solver.parameters.num_workers = 16
solver.parameters.log_to_stdout = False

with open("cp_sat_log.txt", "w") as text_file:
    solver.log_callback = lambda str: text_file.write(f"{str}\n")
    status = solver.solve(model, solution_printer)


# Best solution:
if status == cp_model.OPTIMAL or status == cp_model.FEASIBLE:
    if status == cp_model.OPTIMAL:
        print("Optimal solution:")
    else:
        print("Non-optimal solution:")

    """ Dumb result dump"""
    with open("results.md", "w") as text_file:
        for q in quêtes:
            result = ""
            for b in bénévoles:
                if solver.value(assignations[(b, q)]) == 1:
                    app = appréciation_dune_quête(b, q)
                    if result == "":
                        result = f"{b} ({app})"
                    else:
                        result = f"{result}, {b} ({app})"
            text_file.write(f"Quête {q}: {result}\n")
        max_diff = 0
        max_diff_abs = 0
        for b in bénévoles:
            minutes = solver.value(sum(temps_total_bénévole(b, assignations).values()))
            diff = solver.value(sum(diff_temps(b, assignations).values()))
            if abs(diff) > max_diff_abs:
                max_diff = diff
                max_diff_abs = abs(diff)
            text_file.write(
                f"{b.surnom}: {int(minutes // 60):0=2d}h{int(minutes % 60):0=2d} ({diff/60:.1f})\n"
            )
    print(
        f"Objective value = {solver.objective_value}",
    )
    print(f"Deviation horaire maximale = {max_diff}")
else:
    print("No solution found !")


""" Quelques données sur les quêtes """


def total_temps_travail(quêtes: List[Quête]):
    return sum(q.durée_minutes() * q.nombre_bénévoles for q in quêtes)


def total_temps_dispo(bénévoles: List[Bénévole]):
    return sum(b.heures_théoriques * 60 for b in bénévoles)


temps_total = total_temps_travail(quêtes)
temps_dispo = total_temps_dispo(bénévoles)
print()
print(
    f"Temps de travail total: {int(temps_total // 60):0=2d}h{int(temps_total % 60):0=2d}"
)
print(
    f"Temps de travail disponible: {int(temps_dispo // 60):0=2d}h{int(temps_dispo % 60):0=2d}"
)

# Statistics.
print("\nStatistics")
print(f"- conflicts: {solver.num_conflicts}")
print(f"- branches : {solver.num_branches}")
print(f"- wall time: {solver.wall_time}s")


if status == cp_model.INFEASIBLE:
    exit()

result: Dict[Quête, List[Bénévole]] = {}
for q in quêtes:
    participants = []
    for b in bénévoles:
        if solver.value(assignations[(b, q)]):
            participants.append(b)
    result[q] = participants

from export_json import write_json

write_json(result)

"""
  Autres contraintes:
  - [x] Respect temps horaire quotidien
  - [x] Une pause de 4-5 heures consécutive chaque jour
  - [ ] Des activités différentes chaque jour ?
  - [ ] Des horaires différents chaque jour ?
  - [x] La sérénité
  - [x] Les horaires indispo
  - [x] Les horaires de prédilection
  - [ ] Équilibrer les déficits ou les excès, notamment de la "satisfaction"
  - [x] Pause de 15 minutes entre deux missions qui ne sont pas dans le même lieu
  - [x] Sur les scènes, on veut que les tâches consécutives soit si possible faites par les mêmes personnes
  - [ ] A la fin de la semaine, c'est cool si tout le monde a fait chaque type de quêtes
"""

""" ICAL export """
from icalendar import Calendar, Event

cal = Calendar()
cal.add("prodid", "-//LBC Calendar//mxm.dk//")
cal.add("version", "2.0")

for q in quêtes:
    result = ""
    for b in bénévoles:
        if solver.value(assignations[(b, q)]) == 1:
            app = appréciation_dune_quête(b, q)
            smile = smile_of_appréciation(app)
            if result == "":
                result = f"{b} {smile}"
            else:
                result = f"{result}, {b} {smile}"
    event = Event()
    event.add("summary", f"{result}: {q.nom}")
    lieu = "partout"
    if q.lieu:
        lieu = q.lieu.nom
    event.add("description", f"Lieu: {lieu}")
    event.add("dtstart", q.début)
    event.add("dtend", q.fin)
    event.add("dtstamp", q.fin)
    cal.add_component(event)

import os

path = os.path.join(os.getcwd(), "example.ics")
f = open(path, "wb")
f.write(cal.to_ical())
f.close()

print(path)
