from __future__ import annotations  # allows class type usage in class decl
from typing import List, Dict

"""  ^^^^^^^^^^  """

import csv, re, sys

from datetime import date, time, datetime

from ortools.sat.python import cp_model


""" Sales types """


class Lieu:
    tous: Dict[str, Lieu] = {}

    def __init__(self, nom):
        self.nom = nom
        Lieu.tous[self.nom] = self


class Type_de_quête:
    tous: Dict[str, Type_de_quête] = {}

    def __init__(self, nom):
        self.nom = nom
        Type_de_quête.tous[self.nom] = self


class Quête:
    toutes: List[Quête] = []
    par_jour: Dict[date, List[Quête]] = {}

    def __init__(self, nom, type, lieu, nombre_bénévoles, début, fin):
        self.nom: str = nom
        self.type: str = type
        self.lieu: Lieu = lieu
        self.nombre_bénévoles: int = nombre_bénévoles
        self.début: datetime = début
        self.fin: datetime = fin
        Quête.toutes.append(self)
        quêtes_du_jour = Quête.par_jour.get(self.début.date, [])
        Quête.par_jour[self.début.date] = quêtes_du_jour.append(self)

    def __str__(self) -> str:
        return f"{self.nom}, {self.début.strftime('%a %H:%M')} -> {self.fin.strftime('%H:%M')}"

    def durée_minutes(self) -> int:
        return (self.fin - self.début).total_seconds() / 60

    def en_même_temps(self) -> filter[Quête]:
        """Return la liste de toutes les quêtes chevauchant celle-ci. Cette liste
        inclue la quête courante."""
        return filter(
            lambda q2: (self.début <= q2.début and self.fin > q2.début)
            or (self.fin >= q2.fin and self.début < q2.fin),
            Quête.toutes,
        )


class Bénévole:
    """Classe permettant de gérer les bénévoles et maintenant à jour une liste de tous les bénévoles

    Les bénévoles sont identifiés par leur surnom qui doit donc être unique."""

    tous: Dict[str, Bénévole] = {}

    def __init__(self, surnom, prénom, nom, heures_theoriques):
        self.surnom: str = surnom if surnom else prénom
        self.prénom: str = prénom
        self.nom: str = nom
        self.heures_theoriques: int = heures_theoriques
        self.score_types_de_quêtes: Dict[Type_de_quête, int] = {}
        self.binômes_interdits: List[Bénévole] = []
        self.lieux_interdits: List[Lieu] = []
        try:
            print(Bénévole.tous[self.surnom])
            print(
                "Le nom (ou le surnom) des bénévoles doit être unique. Deux bénévoles on le nom",
                self.surnom,
            )
        except KeyError:
            Bénévole.tous[self.surnom] = self

    def __hash__(self):
        return hash(self.surnom)

    def __cmp__(self, lautre):
        return str.__cmp__(self.surnom, lautre.surnom)

    def __eq__(self, lautre):
        return self.__cmp__(lautre) == 0

    def __str__(self) -> str:
        return f"{self.surnom}"

    def equal(self, lautre):
        self.surnom == lautre.surnom

    def appréciation_dune_quête(self, quête):
        return self.score_types_de_quêtes.get(quête.type, 0)

    def appréciation_du_planning(self, planning):
        for (b, q, n), _ in enumerate(filter(lambda q: q == 1, planning)):
            if self == Bénévole.tous[b]:
                print(q, n)


""" Importation des données """
csv_lieux = sys.argv[1]  # That's not very flexible...
csv_types_de_quete = sys.argv[2]
csv_bénévoles = sys.argv[3]
csv_quêtes = sys.argv[4]

encoding = "utf-8-sig"
dialect = csv.excel  # Dialecte par défault
with open(csv_bénévoles, newline="", encoding=encoding) as csvfile:
    # We expect the dialect to be the same for every input csv
    dialect = csv.Sniffer().sniff(csvfile.read(1024))

with open(csv_lieux, newline="", encoding=encoding) as csvfile:
    reader = csv.DictReader(csvfile, dialect=dialect)
    for row in reader:
        Lieu(row["Name"])

with open(csv_types_de_quete, newline="", encoding=encoding) as csvfile:
    reader = csv.DictReader(csvfile, dialect=dialect)
    for row in reader:
        Type_de_quête(row["Name"])

with open(csv_bénévoles, newline="", encoding=encoding) as csvfile:
    reader = csv.DictReader(csvfile, dialect=dialect)
    for row in reader:
        Bénévole(
            row["Name"],
            row["Prénom"],
            row["Full Name"],
            int(row["heures théoriques par jour"]),
        )


def parse_horaires(horaire: str):
    horaire_groups = re.match(r"(.*) \(.*\) → (.*) \(.*\)", horaire)
    if horaire_groups:
        début = datetime.strptime(horaire_groups.group(1), "%d/%m/%Y %H:%M")
        fin = datetime.strptime(horaire_groups.group(2), "%d/%m/%Y %H:%M")
        return début, fin
    else:
        horaire_groups = re.match(r"(.*) \(.*\) → (.*)", horaire)
        début = datetime.strptime(horaire_groups.group(1), "%d/%m/%Y %H:%M")
        heure_fin = time.fromisoformat(f"{horaire_groups.group(2)}:00")
        fin = datetime.combine(début, heure_fin)
        return début, fin


with open(csv_quêtes, newline="", encoding=encoding) as csvfile:
    reader = csv.DictReader(csvfile, dialect=dialect)
    for row in reader:
        try:
            place = re.match(r"(.*) \(http", row["Place"]).group(1)
            type_de_quête = re.match(r"(.*) \(http", row["Type de Quete"]).group(1)
            début, fin = parse_horaires(row["Horaire"])
            Quête(
                row["Name"],
                Type_de_quête.tous[type_de_quête],
                Lieu.tous[place],
                int(row["Needed"]),
                début,
                fin,
            )
        except:
            print("Something went wrong while parsing:\n", row)

quêtes = Quête.toutes
bénévoles = Bénévole.tous.values()


def quêtes_dun_lieu(lieu):
    return filter(lambda q: q.lieu == lieu, quêtes)


"""Préparation du modèle et des contraintes"""

model = cp_model.CpModel()

""" On créé une variable par bénévole pour chaque "slot" de chaque quête."""
assignations: Dict[(Bénévole, Quête, int), cp_model.BoolVarT] = {}
for b in bénévoles:
    for q in quêtes:
        for n in range(q.nombre_bénévoles):
            assignations[(b, q, n)] = model.new_bool_var(f"shift_b{b}_q{q}_n{n}")

""" Tous les slots de toutes les quêtes doivent être peuplés """
for q in quêtes:
    for n in range(0, q.nombre_bénévoles):
        model.add_exactly_one(assignations[(b, q, n)] for b in bénévoles)

""" Un même bénévole ne peut pas remplir plusieurs quêtes en même temps """
for b in bénévoles:
    for q in quêtes:
        model.add_at_most_one(
            assignations[(b, q_en_même_temps, n)]
            for q_en_même_temps in q.en_même_temps()
            for n in range(q_en_même_temps.nombre_bénévoles)
        )

""" Les lieux interdits sont interdits """
for b in bénévoles:
    for lieu in b.lieux_interdits:
        for q in quêtes_dun_lieu(lieu):
            for n in range(q.nombre_bénévoles):
                model.add(assignations[(b, q, n)] == 0)


""" Ils se détestent, séparez-les ! """
for b in bénévoles:
    for e in b.binômes_interdits:
        for q in quêtes:
            for n1 in range(q.nombre_bénévoles):
                for n2 in range(q.nombre_bénévoles):
                    if n1 != n2:
                        model.add_implication(
                            assignations[(b, q, n1)],
                            assignations[(e, q, n2)].Not(),
                        )


""" Calcul de la qualité d'une reponse """


def temps_total_bénévole(b):
    return sum(
        q.durée_minutes() * assignations[(b, q, n)]
        for q in quêtes
        for n in range(q.nombre_bénévoles)
    )


model.maximize(
    sum(
        b.appréciation_dune_quête(q) * assignations[(b, q, n)]
        for b in bénévoles
        for q in quêtes
        for n in range(q.nombre_bénévoles)
    )
)


class VarArraySolutionPrinter(cp_model.CpSolverSolutionCallback):
    """Print intermediate solutions."""

    def __init__(self, assignations):
        cp_model.CpSolverSolutionCallback.__init__(self)
        self._assignations = assignations
        self._solution_count = 0

    def on_solution_callback(self) -> None:
        self._solution_count += 1
        print(f"Solution {self._solution_count}")
        for q in quêtes:
            result = ""
            for n in range(q.nombre_bénévoles):
                for b in bénévoles:
                    if self.value(self._assignations[(b, q, n)]) == 1:
                        if result == "":
                            result = f"{b}"
                        else:
                            result = f"{result}, {b}"
            print(f"Quête {q}: {result}")
        print()

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
    for q in quêtes:
        result = ""
        for n in range(q.nombre_bénévoles):
            for b in bénévoles:
                if solver.value(assignations[(b, q, n)]) == 1:
                    if result == "":
                        result = f"{b}"
                    else:
                        result = f"{result}, {b}"
        print(f"Quête {q}: {result}")
    print(
        f"Obective value = {solver.objective_value}",
    )
else:
    print("No optimal solution found !")


# Statistics.
print("\nStatistics")
print(f"- conflicts: {solver.num_conflicts}")
print(f"- branches : {solver.num_branches}")
print(f"- wall time: {solver.wall_time}s")
