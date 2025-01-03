from __future__ import annotations  # allows class type usage in class decl
from typing import List, Dict
from datetime import date, time, datetime, timedelta
from operator import contains
import os, sys, math, random
from ortools.sat.python import cp_model
from data_model import Bénévole, Type_de_quête, Quête, Spectacle, strengthen
from export_json import write_json

# prepare log folder and file
date_now = datetime.now().strftime("%Y%m%d %Hh%Mm%Ss")
log_folder = f"runs/{date_now}"
if not os.path.exists(f"{log_folder}/solutions"):
    os.makedirs(f"{log_folder}/solutions")

log_file_path = f"{log_folder}/log.txt"
log_file = open(log_file_path, "w")


def print(str="\n"):
    log_file.write(f"{str}\n")
    sys.stdout.write(f"{str}\n")


# open log file

""" Data import """
# from import_json import from_file

# from_file("data/db.json")
from import_gapi import main

main()

# Once all the data is loaded, resolve references:
strengthen()

print("Liste des types quêtes:\n")
for q in Type_de_quête.tous.values():
    print(q.détails())

print("Liste des bénévoles:\n")
for b in Bénévole.tous.values():
    print(b.détails())
    print()

print("Liste des quêtes:\n")
for q in Quête.toutes:
    print(q.détails())
    print()

quêtes = sorted(Quête.toutes)
bénévoles = Bénévole.tous.values()

""" Outils """


def member_f(l, f):
    for e in l:
        if f(e):
            return True
    return False


def member(l, x):
    for e in l:
        if e == x:
            return True
    return False


def quêtes_dun_lieu(lieu):
    return filter(lambda q: q.lieu == lieu, quêtes)


def quêtes_dun_type(t):
    return filter(lambda q: member(q.types, t), quêtes)


def time_to_minutes(t: time):
    return t.hour * 60 + t.minute


def diff_minutes(t1: time, t2: time):
    return time_to_minutes(t2) - time_to_minutes(t1)


def print_duration(minutes):
    return f"{int(minutes // 60):0=2d}h{int(minutes % 60):0=2d}"


def print_signed_duration(minutes):
    if minutes >= 0:
        return f"+{print_duration(minutes)}"
    else:
        return f"-{print_duration(abs(minutes))}"


# id_quête_sérénité = "784fc134-cab5-4797-8be2-7a7a91e57795"

# id_tdg_clean = "9f95caf2-a32b-4454-9994-6ce17a5e75e6"
# id_tdg_suivi = "78250bf9-fb52-41ac-af5b-879d2ca7ff1c"

# id_tdq_gradinage = "987ad365-0032-42c6-8455-8fbf66d6179d"

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
    model.add(
        sum(assignations[(b, q)] for b in bénévoles) == q.nombre_bénévoles
    ).with_name(f"tout_est_rempli_{b}_{q}")


""" Un même bénévole ne peut pas remplir plusieurs quêtes en même temps """

for q in quêtes:
    en_même_temps = list(q.en_même_temps())
    for q2 in en_même_temps:
        for b in bénévoles:
            if q != q2:
                model.add_at_most_one(
                    [assignations[(b, q)], assignations[(b, q2)]]
                ).with_name(f"en_meme_temps_{b}_{q}_{q2}")

""" Certaines quêtes sont déjà assignées """
for q in quêtes:
    for b in q.bénévoles:
        model.add(assignations[(b, q)] == 1).with_name(f"fixé_{b}_{q}")

""" Et certains bénévoles ne devrait rien faire d'autre """
for d, quêtes_du_jour in Quête.par_jour.items():
    for b in bénévoles:
        if b.est_assigné(d):
            for q in quêtes_du_jour:
                if not (member(q.bénévoles, b)):
                    model.add(assignations[(b, q)] == 0).with_name(
                        f"assigné_peut_pas_faire_{b}_{q}"
                    )

""" On aimerait que tout le monde participe à certaines tâches """


def tout_le_monde_fait(t: Type_de_quête):
    for b in bénévoles:
        assigné = True
        for d in Quête.par_jour.keys():
            assigné = assigné and b.est_assigné(d)
        if not (assigné) and not (member(b.types_de_quête_interdits, t)):
            # Todo there are more checks to do here such has place interdiction
            # print(f"{b} fait du clean")
            model.add_at_least_one(
                assignations[(b, q)] for q in quêtes_dun_type(t)
            ).with_name(f"un_seul_{b}_{t}")


# tout_le_monde_fait(Type_de_quête.tous[id_tdg_suivi])


""" On aimerait certaines tâches soient faites par un maximum de personnes différentes """


def un_max_de_monde_fait(t: Type_de_quête):
    for b in bénévoles:
        assigné = True
        for d in Quête.par_jour.keys():
            assigné = assigné and b.est_assigné(d)
        if not (assigné) and not (member(b.types_de_quête_interdits, t)):
            # Todo there are more checks to do here such has place interdiction
            model.add_at_most_one(
                assignations[(b, q)] for q in quêtes_dun_type(t)
            ).with_name(f"au_plus_un_{b}_{t}")


# un_max_de_monde_fait(Type_de_quête.tous[id_tdg_clean])

""" Un même bénévole ne fait pas plusieurs fois le suivi du même spectacle """

# quêtes_suivi_des_spectacles: Dict[str, List[Quête]] = {}
# for q in quêtes:
#     if member_f(q.types, lambda t: t.id == id_tdg_suivi):
#         quêtes_spe = quêtes_suivi_des_spectacles.get(q.spectacle.id, [])
#         if quêtes_spe == []:
#             quêtes_suivi_des_spectacles[q.spectacle.id] = [q]
#         else:
#             quêtes_spe.append(q)

# for b in bénévoles:
#     for quêtes_spe in quêtes_suivi_des_spectacles.values():
#         model.add_at_most_one(assignations[(b, q)] for q in quêtes_spe).with_name(
#             f"no_double_show_{b}_{quêtes_spe[0]}"
#         )

""" Les tâches consécutives d'une scène sont faites par les mêmes bénévoles """

# quêtes_liées_des_spectacles: List[List[Quête]] = []
# for date, quêtes_du_jour in Quête.par_jour.items():
#     quêtes_des_spectacles: Dict[str, List[Quête]] = {}
#     for q in quêtes_du_jour:
#         if q.spectacle and not (member_f(q.types, lambda t: t.id == id_tdq_gradinage)):
#             quêtes_du_spectacle = quêtes_des_spectacles.get(q.spectacle.id, [])
#             if quêtes_du_spectacle == []:
#                 quêtes_des_spectacles[q.spectacle.id] = [q]
#             else:
#                 quêtes_du_spectacle.append(q)
#     for l in quêtes_des_spectacles.values():
#         quêtes_liées_des_spectacles.append(l)


# def suivi_quêtes_dun_spectacles(quêtes: List[Quête]):
#     min_nb = 99
#     min_quête = None
#     for q in quêtes:
#         if q.nombre_bénévoles < min_nb:
#             min_nb = q.nombre_bénévoles
#             min_quête = q

#     # On s'assure que les quêtes groupées ne se chevauchent pas, cela évite des
#     # erreurs incompréhensibles:
#     for q in quêtes:
#         for q2 in q.en_même_temps():
#             for q3 in quêtes:
#                 if q != q3 and q2 == q3:
#                     print("Arg, des quêtes groupées se chevauchent:")
#                     print(f"{q} chevauche {q3}")
#                     exit()

#     for b in bénévoles:
#         # Tout bénévole participant à la quête qui requiert le moins de bénévoles
#         # dans le groupe de quêtes doit participer aux autres quêtes du groupe.
#         model.add_bool_and(assignations[(b, q)] for q in quêtes).only_enforce_if(
#             assignations[(b, min_quête)]
#         ).with_name(f"suivi_{b}_{quêtes[0]}")


# for quêtes_dun_spectacle in quêtes_liées_des_spectacles:
#     suivi_quêtes_dun_spectacles(quêtes_dun_spectacle)

""" Certains bénévoles ne sont pas là pendant la totalité de l'évènement """
for b in bénévoles:
    for q in quêtes:
        if b.date_arrivée:
            # On vérifie que ce n'est pas une quête forcée:
            if not (contains(q.bénévoles, b)):
                if q.début < b.date_arrivée:
                    explain_var = model.new_bool_var(f"{b} pas encore arrivé pour {q}")
                    model.add_assumption(explain_var)
                    model.add(assignations[(b, q)] == 0).with_name(
                        f"before_arrival_{b}_{q}"
                    ).only_enforce_if(explain_var)
        if b.date_départ:
            # On vérifie que ce n'est pas une quête forcée:
            if not (contains(q.bénévoles, b)):
                if q.fin > b.date_départ:
                    explain_var = model.new_bool_var(f"{b} déjà parti pour {q}")
                    model.add_assumption(explain_var)
                    model.add(assignations[(b, q)] == 0).with_name(
                        f"after_leave_{b}_{q}"
                    ).only_enforce_if(explain_var)

""" Certains bénévoles sont indisponibles à certains horaires """
for b in bénévoles:
    for date, quêtes_du_jour in Quête.par_jour.items():
        for q in quêtes_du_jour:
            # On vérifie que ce n'est pas une quête forcée:
            if not (contains(q.bénévoles, b)):
                for début_indispo in b.indisponibilités:
                    # On compare des datetime pour éviter les erreurs bêtes:
                    début_indispo = datetime.combine(
                        date, début_indispo, q.début.tzinfo
                    )
                    if début_indispo.hour < 5:  # TODO: check ? configurable ?
                        début_indispo += timedelta(days=1)
                    fin_indispo = début_indispo + timedelta(hours=1)

                    if not (fin_indispo <= q.début or début_indispo >= q.fin):
                        explain_var = model.new_bool_var(
                            f"{b} indisponible pour {q}\n({q.début} -> {q.fin}) \n({début_indispo} -> {fin_indispo})"
                        )
                        model.add_assumption(explain_var)
                        model.add(assignations[(b, q)] == 0).with_name(
                            f"indispo_{b}_{q}"
                        ).only_enforce_if(explain_var)
                        break

""" Tout le monde ne peut pas assumer les quêtes sérénité """
# for b in bénévoles:
#     if not (b.sérénité):
#         for q in quêtes:
#             if contains(q.types, Type_de_quête.tous[id_quête_sérénité]):
#                 model.add(assignations[(b, q)] == 0).with_name(f"pas_serein_{b}_{q}")

""" Certaines quêtes sont réservées aux spécialistes """
for q in quêtes:
    spécialités_requises = set(filter(lambda tdq: tdq.spécialiste_only, q.types))
    for b in bénévoles:
        if not (spécialités_requises.issubset(b.spécialités)):
            explain_var = model.new_bool_var(
                f"{b} ne peut pas assumer {q} (un(e) spécialiste est requis)"
            )
            model.add_assumption(explain_var)
            model.add(assignations[(b, q)] == 0).with_name(
                f"pas_spécialiste_{b}_{q}"
            ).only_enforce_if(explain_var)


""" Les lieux interdits sont interdits """
for b in bénévoles:
    for lieu in b.lieux_interdits:
        for q in quêtes_dun_lieu(lieu):
            explain_var = model.new_bool_var(
                f"{b} ne peut pas assumer {q} (lieu interdit: {lieu})"
            )
            model.add_assumption(explain_var)
            model.add(assignations[(b, q)] == 0).with_name(
                f"lieu_interdit_{b}_{q}"
            ).only_enforce_if(explain_var)

""" Les quêtes interdites sont interdites """
for b in bénévoles:
    for t in b.types_de_quête_interdits:
        for q in quêtes_dun_type(t):
            explain_var = model.new_bool_var(
                f"{b} ne peut pas assumer {q} (type de quête interdit: {t})"
            )
            model.add_assumption(explain_var)
            model.add(assignations[(b, q)] == 0).with_name(
                f"tdq_interdit_{b}_{q}"
            ).only_enforce_if(explain_var)


""" Ils se détestent, séparez-les ! """
inimitiés: Dict[Bénévole, set[Bénévole]] = {}
for b in bénévoles:
    inimitiés[b] = set(b.binômes_interdits)
    for e in b.binômes_interdits:
        if b not in inimitiés.get(e, set()):
            for q in quêtes:
                explain_var = model.new_bool_var(
                    f"{b} ne peut pas travailler avec {e} lors de {q}"
                )
                model.add_assumption(explain_var)
                model.add(assignations[(b, q)] + assignations[(e, q)] <= 1).with_name(
                    f"blaire_pas_{b}_{e}_{q}"
                ).only_enforce_if(explain_var)

""" Chacun a un trou dans son emploi du temps """


début_période_pause = 9 * 60  # minutes
fin_période_pause = 23 * 60  # minutes
durée_pause_min = 5 * 60  # minutes


# TODO: la pause devrait durer plus longtemps et pouvoir commencer à n'importe
# quel moment
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
        explain_var = model.new_bool_var(
            f"{b} doit avoir une pause de {durée_pause_min / 60}h le {date}"
        )
        model.add_assumption(explain_var)

        # La pause est suffisamment longue:
        model.add(size >= durée_pause_min).with_name(
            f"pause_{size}_>=_{durée_pause_min}"
        ).only_enforce_if(explain_var)

        # Le bénévole n'a aucune quête pendant sa pause:
        overlaps = [interval_pause]
        for q in quêtes:
            overlaps.append(intervalles[(b, q)])
        model.add_no_overlap(overlaps).with_name(
            f"noverlap_{b}_{date}"
        )  # "only_enforce_if" doesn't work with that constraint


for b in bénévoles:
    c_est_la_pause(b)


""" Calcul de la qualité d'une réponse """

""" Contrôle du temps de travail """


def temps_total_quêtes(quêtes: List[Quête]):
    return sum(q.durée_minutes() * q.nombre_bénévoles for q in quêtes)


# Temps de travail d'un bénévole sur un ensemble de quêtes
def temps_bev(b, quêtes, assignations):
    # assignations[(b, q)] pour un bénévole b et une quête q vaut 0 ou 1 et
    # indique si le bénévole a été assigné à cette quête.
    return sum(q.durée_minutes() * assignations[(b, q)] for q in quêtes)


# Temps de travail, par jour, d'un bénévole
def temps_quotidien_bénévole(b, assignations):
    # On renvoie un dictionnaire date -> temps de travail
    return {
        date: temps_bev(b, quêtes, assignations)
        for date, quêtes in Quête.par_jour.items()
    }


def temps_travail_théorique(date, b: Bénévole):
    if (not (b.date_arrivée) or date >= b.date_arrivée.date()) and (
        not (b.date_départ) or date < b.date_départ.date()
    ):
        return b.heures_théoriques * 60
    else:
        return 0


def temps_quotidien_théorique_bénévole(b):
    # On renvoie un dictionnaire date -> temps de travail
    return {
        date: temps_travail_théorique(date, b)
        for date, _quêtes in Quête.par_jour.items()
    }


# Écart de l'écart du temps de travail d'un bénévole par rapport à la moyenne
# Renvoie un dictionnaire indexé par les jours
def diff_temps(b, assignations):
    return {
        date: (tdt - (temps_travail_théorique(date, b)))
        for date, tdt in temps_quotidien_bénévole(b, assignations).items()
    }


""" Équilibrage du temps de travail """


def bornage_des_excès(bénévoles, écart_quotidien_max=30):
    """The goal is to minimize work excess differences on the whole event by allowing some day-to-day differences below the threshold."""
    max_tdt = 60 * max([b.heures_théoriques for b in Bénévole.tous.values()])
    borne_inf = model.new_int_var(-2 * max_tdt, 0, "borne_inf_des_diffs")
    borne_sup = model.new_int_var(0, 2 * max_tdt, "borne_sup_des_diffs")
    for b in bénévoles:
        diff_par_jour = diff_temps(b, assignations)
        for d, diff in diff_par_jour.items():
            model.add(diff <= écart_quotidien_max).with_name(f"écart_{b}_{d}")
        diff = sum(diff for _, diff in diff_par_jour.items())
        model.add(diff <= borne_sup).with_name(f"sup_{b}")
        model.add(diff >= borne_inf).with_name(f"inf_{b}")
    return borne_sup - borne_inf


""" Pondération des préférences des bénévoles """


def appréciation_dune_quête(bénévole: Bénévole, quête: Quête):
    # On découpe la quête par blocs de 15 minutes
    acc = quête.début
    somme_prefs = 0
    while acc < quête.fin:
        time = acc.time()
        for pref_t, p in bénévole.pref_horaires.items():
            if time.hour == pref_t.hour:
                somme_prefs += p
                break
        acc = min(acc + timedelta(minutes=15), quête.fin)
    return somme_prefs


def appréciation_du_planning(bénévole: Bénévole, quêtes: List[Quête]):
    return sum(
        assignations[(bénévole, q)] * (appréciation_dune_quête(bénévole, q))
        for q in quêtes
    )


# This might not always be satisfiable
for b in bénévoles:
    for q in quêtes:
        model.add(appréciation_dune_quête(b, q) >= 0).with_name(f"appréciation_{b}_{q}")

""" Distance entre la première et la dernière quête """


def amplitude_horaire(b: Bénévole, quêtes: List[Quête]):
    début = model.new_int_var(0, 60 * 24, f"début_journée_{b}")
    fin = model.new_int_var(0, 60 * 24, f"fin_journée_{b}")
    model.add_min_equality(
        début, map(lambda q: intervalles[(b, q)].start_expr(), quêtes)
    ).with_name(f"amp_min_{b}")
    model.add_max_equality(
        fin, map(lambda q: intervalles[(b, q)].end_expr(), quêtes)
    ).with_name(f"amp_max_{b}")
    return fin - début


def amplitudes(b: Bénévole):
    return sum(
        amplitude_horaire(b, quêtes) - (b.heures_théoriques * 60)
        for quêtes in Quête.par_jour.values()
    )


""" Formule finale """

model.minimize(
    10 * bornage_des_excès(bénévoles)
    # + 0.5 * sum(amplitudes(b) for b in bénévoles)
    - sum(appréciation_du_planning(b, quêtes) for b in bénévoles)
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


def dumb_dump(file, assignations):
    with open(file, "w") as text_file:
        max_diff = 0
        max_diff_abs = 0
        total_diff = 0
        all = []
        for b in bénévoles:
            tdt = sum(temps_quotidien_bénévole(b, assignations).values())
            tdt_théorique = sum(
                temps_travail_théorique(d, b) for d in Quête.par_jour.keys()
            )
            tdt_ajusté = sum(
                temps_travail_théorique(d, b) for d in Quête.par_jour.keys()
            )
            diff = tdt - tdt_ajusté
            total_diff += diff
            if abs(diff) > max_diff_abs:
                max_diff = diff
                max_diff_abs = abs(diff)
            all.append(
                {
                    "d": diff,
                    "s": f"{b.surnom}:\t{print_duration(tdt)} / {print_duration(tdt_ajusté)} / {print_duration (tdt_théorique)}\t({print_signed_duration(diff)})\n",
                }
            )
        text_file.write(f"\nMax diff: {print_duration(max_diff)}\n")
        text_file.write(f"Total diff: {print_signed_duration(total_diff)}\n\n")
        all.sort(key=lambda l: l["d"], reverse=True)
        for l in all:
            text_file.write(f"{l["s"]}")


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

        if not os.path.exists(f"{log_folder}/solutions/{self._solution_count:0=3d}"):
            os.makedirs(f"{log_folder}/solutions/{self._solution_count:0=3d}")

        print(f"Solution {self._solution_count:0=3d}:\n\t{écarts_line}\n\t{smile_line}")
        dumb_dump(
            f"{log_folder}/solutions/{self._solution_count:0=3d}/results.md",
            assignations_val,
        )

        result: Dict[Quête, List[Bénévole]] = {}
        for q in quêtes:
            participants = []
            for b in bénévoles:
                if assignations_val[(b, q)]:
                    participants.append(b)
            result[q] = participants
        write_json(
            result, file=f"{log_folder}/solutions/{self._solution_count:0=3d}/results"
        )

    @property
    def solution_count(self) -> int:
        return self.__solution_count


# Enumerate all solutions.
solver = cp_model.CpSolver()
solution_printer = VarArraySolutionPrinter(assignations)
solver.parameters.log_search_progress = True
solver.parameters.num_workers = 10
solver.parameters.log_to_stdout = False

with open(f"{log_folder}/cp_sat_log.txt", "w") as text_file:
    solver.log_callback = lambda str: text_file.write(f"{str}\n")
    status = solver.solve(model, solution_printer)


# Best solution:
if status == cp_model.OPTIMAL or status == cp_model.FEASIBLE:
    if status == cp_model.OPTIMAL:
        print("Optimal solution:")
    else:
        print("Non-optimal solution:")

    """ Dumb result dump"""
    assignations_values = {k: solver.value(v) for k, v in assignations.items()}
    dumb_dump(f"{log_folder}/results.md", assignations_values)

    print(
        f"Objective value = {solver.objective_value}",
    )
else:
    print("Aucune solution trouvée. Raisons possibles:")
    # https://github.com/google/or-tools/issues/973#issuecomment-718220753
    # Added "enforcement variables" that are assumed to be TRUE allows us to
    # provide an explanation in some cases. An example of this is the "Ils se
    # détestent, séparez-les !" constraint.
    # Aucune solution trouvée. Raisons possibles:
    # Ulysse ne peut pas travailler avec La punaise
    for index in solver.sufficient_assumptions_for_infeasibility():
        print(f"- {model.proto.variables[index].name}")


""" Quelques données sur les quêtes """


def total_temps_dispo_par_jour(date, bénévoles):
    return sum(temps_quotidien_théorique_bénévole(b)[date] for b in bénévoles)


def total_temps_dispo(bénévoles):
    return sum(
        sum(v for v in temps_quotidien_théorique_bénévole(b).values())
        for b in bénévoles
    )


temps_total = temps_total_quêtes(quêtes)
temps_dispo = total_temps_dispo(bénévoles)
print()

for d, qs in Quête.par_jour.items():
    temps = temps_total_quêtes(qs)
    dispo = total_temps_dispo_par_jour(d, bénévoles)
    print(
        f"Temps de travail {d} : {int(temps // 60):0=2d}h{int(temps % 60):0=2d}, disponible: {int(dispo // 60):0=2d}h{int(dispo % 60):0=2d}"
    )
print(
    f"Temps de travail total: {int(temps_total // 60):0=2d}h{int(temps_total % 60):0=2d}, disponible: {int(temps_dispo // 60):0=2d}h{int(temps_dispo % 60):0=2d}"
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

write_json(result, file=f"{log_folder}/results.json")

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

path = os.path.join(os.getcwd(), f"{log_folder}/events.ics")
f = open(path, "wb")
f.write(cal.to_ical())
f.close()

log_file.close()
