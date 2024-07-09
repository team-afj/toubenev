from __future__ import annotations  # allows class type usage in class decl
from typing import List, Dict
from datetime import date, time, datetime, timedelta


""" Sales types """


class Lieu:
    tous: Dict[str, Lieu] = {}

    def __init__(self, nom):
        self.nom = nom
        Lieu.tous[self.nom] = self


class Type_de_quête:
    tous: Dict[str, Type_de_quête] = {}

    def __init__(self, nom, sécable):
        self.nom = nom
        self.sécable: bool = sécable
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

        date_début = self.début.date()
        if self.début.time() < time(4):
            # Day starts at 4 am
            date_début = date_début - timedelta(1)

        quêtes_du_jour: List[Quête] = Quête.par_jour.get(date_début, [])
        if quêtes_du_jour == []:
            Quête.par_jour[date_début] = [self]
        else:
            quêtes_du_jour.append(self)

    def __str__(self) -> str:
        return f"{self.nom}, {self.début.strftime('%a %H:%M')} -> {self.fin.strftime('%H:%M')}"

    def durée_minutes(self) -> int:
        return int((self.fin - self.début).total_seconds() / 60)

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

    def __init__(
        self, surnom, prénom, nom, heures_théoriques, indisponibilités, pref_horaires
    ):
        self.surnom: str = surnom if surnom else prénom
        self.prénom: str = prénom
        self.nom: str = nom
        self.heures_théoriques: int = heures_théoriques
        self.score_types_de_quêtes: Dict[Type_de_quête, int] = {}
        self.binômes_interdits: List[Bénévole] = []
        self.lieux_interdits: List[Lieu] = []
        self.indisponibilités: List[time] = indisponibilités
        self.pref_horaires: Dict[time, int] = pref_horaires

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

    # def appréciation_dune_quête(self, quête):
    #     return self.score_types_de_quêtes.get(quête.type, 0)

    def appréciation_du_planning(self, planning):
        for (b, q, n), _ in enumerate(filter(lambda q: q == 1, planning)):
            if self == Bénévole.tous[b]:
                print(q, n)
