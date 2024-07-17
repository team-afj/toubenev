from __future__ import annotations  # allows class type usage in class decl
from typing import List, Dict
from datetime import date, time, datetime, timedelta
from operator import contains
import math
from ortools.sat.python import cp_model
from data_model import Bénévole, Lieu, Type_de_quête, Quête

# import import_csv
from import_json import from_file

from_file("data/db.json")

quêtes = sorted(Quête.toutes)
bénévoles = Bénévole.tous.values()


def quêtes_dun_lieu(lieu):
    return filter(lambda q: q.lieu == lieu, quêtes)


id_quête_sérénité = "784fc134-cab5-4797-8be2-7a7a91e57795"

"""Préparation du modèle et des contraintes"""

model = cp_model.CpModel()

""" On créé une variable par bénévole pour chaque "slot" de chaque quête."""
assignations: Dict[(Bénévole, Quête, int), cp_model.BoolVarT] = {}
for b in bénévoles:
    for q in quêtes:
        assignations[(b, q)] = model.new_bool_var(f"shift_b{b}_q{q}")

""" Tous les slots de toutes les quêtes doivent être peuplés """
for q in quêtes:
    model.add(sum(assignations[(b, q)] for b in bénévoles) == q.nombre_bénévoles)

""" Un même bénévole ne peut pas remplir plusieurs quêtes en même temps """
for b in bénévoles:
    for q in quêtes:
        model.add_at_most_one(
            assignations[(b, q_en_même_temps)] for q_en_même_temps in q.en_même_temps()
        )

""" Certaines quêtes sont déjà assignées """
for q in quêtes:
    for b in q.bénévoles:
        model.add(assignations[(b, q)] == 1)


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


def time_to_minutes(t: time):
    return t.hour * 60 + t.minute


def diff_minutes(t1: time, t2: time):
    return time_to_minutes(t2) - time_to_minutes(t1)


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
            # TODO: this is gnééé. Quests that end after midnight are not
            # handled correctly
            d: int = time_to_minutes(q.début.time())
            f: int = min(time_to_minutes(q.fin.time()), fin_période_pause)
            if f > d:
                interval_quête = model.new_optional_interval_var(
                    d, f - d, f, assignations[(b, q)], f"interval_quête_{b}_{q}"
                )
                overlaps.append(interval_quête)
        model.add_no_overlap(overlaps)


for b in bénévoles:
    c_est_la_pause(b)


""" Calcul de la qualité d'une réponse """

""" Contrôle du temps de travail """


# Temps de travail d'un bénévole sur un ensemble de quêtes
def temps_bev(b, quêtes):
    return sum(q.durée_minutes() * assignations[(b, q)] for q in quêtes)


# Temps de travail, par jour, d'un bénévole
def temps_total_bénévole(b) -> Dict[date, cp_model.IntVar]:
    return {date: temps_bev(b, quêtes) for date, quêtes in Quête.par_jour.items()}


# Différence avec le tdt prévu par jour:
def diff_temps(b):
    return {
        date: tdt - (b.heures_théoriques * 60)
        for date, tdt in temps_total_bénévole(b).items()
    }


def squared(id, value):
    var = model.NewIntVar(0, 100000, f"v_squared_{id}")
    diff = model.NewIntVar(-100000, 100000, f"v_{id}")
    model.Add(diff == value)
    model.AddMultiplicationEquality(var, [diff, diff])
    # model.AddAbsEquality(var, diff)
    return var


# Au carré:
diffs: Dict[Bénévole, cp_model.IntVar] = {}
for b in bénévoles:
    diff_par_jour = diff_temps(b)
    diffs[b] = sum(
        squared(f"diff_{date}_béné_{b}", diff) for date, diff in diff_par_jour.items()
    )

# max_diff = model.NewIntVar(0, 100000, f"max_diff")
# model.AddMaxEquality(max_diff, diffs.values())
# écart type ?

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


""" Formule finale """

model.minimize(sum(diffs[b] - appréciation_du_planning(b, quêtes) for b in bénévoles))


""" Solution printer """


def smile_of_appréciation(app):
    smile = "🙂"
    if app >= 2:
        smile = "🤗"
    if app < 0:
        smile = "😰"
    if app < -10:
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
        smiles = {}
        for q in quêtes:
            for b in bénévoles:
                if self.value(self._assignations[(b, q)]) == 1:
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
            n = math.ceil(n * 30 / total_smiles)
            for _ in range(n):
                smile_line = f"{smile_line}{smile}"
        print(f"Solution {self._solution_count}: {smile_line}")

    @property
    def solution_count(self) -> int:
        return self.__solution_count


# Enumerate all solutions.
solver = cp_model.CpSolver()
solution_printer = VarArraySolutionPrinter(assignations)
solver.parameters.log_search_progress = False
solver.parameters.num_workers = 16

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
            minutes = solver.value(sum(temps_total_bénévole(b).values()))
            diff = solver.value(sum(diff_temps(b).values()))
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
  - [ ] Equilibrer les déficits ou les excès
  - [x] Pause de 15 minutes entre deux missions qui ne sont pas dans le même lieu
  - [ ] Sur les scènes, on veut que les tâches consécutives soit si possible faites par les mêmes personnes
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
