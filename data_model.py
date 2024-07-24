from __future__ import annotations  # allows class type usage in class decl
from typing import List, Dict, Optional
from datetime import date, time, datetime, timedelta
import string

""" Paramêtres """

temps_inter_quêtes = 15  # minutes

""" Sales types """


class Spectacle:
    tous: Dict[str, Spectacle] = {}

    def __init__(self, id, nom):
        self.id: str = id
        self.nom = nom
        Spectacle.tous[self.id] = self


class Lieu:
    tous: Dict[str, Lieu] = {}

    def __init__(self, id, nom):
        self.id: str = id
        self.nom = nom
        Lieu.tous[self.id] = self

    def __cmp__(self, lautre):
        return (self.id > lautre.id) - (self.id < lautre.id)

    def __eq__(self, lautre):
        return self.__cmp__(lautre) == 0


class Type_de_quête:
    tous: Dict[str, Type_de_quête] = {}

    def __init__(self, id, nom, sécable):
        self.id: str = id
        self.nom = nom
        self.sécable: bool = sécable
        Type_de_quête.tous[self.id] = self

    def __hash__(self):
        return hash(self.id)

    def __cmp__(self, lautre):
        return (self.id > lautre.id) - (self.id < lautre.id)

    def __eq__(self, lautre):
        return self.__cmp__(lautre) == 0


class Quête:
    toutes: List[Quête] = []
    par_jour: Dict[date, List[Quête]] = {}

    def __init__(
        self,
        id,
        nom,
        types,
        lieu,
        spectacle,
        nombre_bénévoles,
        début,
        fin,
        bénévoles=[],
    ):
        self.id: str = id
        self.nom: str = nom
        self.types: List[Type_de_quête] = types
        self.lieu: Optional[Lieu] = lieu
        self.spectacle: Optional[Spectacle] = spectacle
        self.nombre_bénévoles: int = nombre_bénévoles
        self.bénévoles: List[Bénévole] = bénévoles
        self.début: datetime = début
        self.fin: datetime = fin
        Quête.toutes.append(self)

        date_début = self.début.date()
        if self.début.time() < time(4):
            # Day starts at 4 am
            date_début = date_début - timedelta(days=1)

        quêtes_du_jour: List[Quête] = Quête.par_jour.get(date_début, [])
        if quêtes_du_jour == []:
            Quête.par_jour[date_début] = [self]
        else:
            quêtes_du_jour.append(self)

    def __str__(self) -> str:
        return f"{self.nom}, {self.début.strftime('%a %H:%M')} -> {self.fin.strftime('%H:%M')}"

    def __lt__(self, other):
        return self.début < other.début

    def durée_minutes(self) -> int:
        return int((self.fin - self.début).total_seconds() / 60)

    def chevauche(self, q2: Quête):
        """Les quêtes qui ne n'ont pas lieu dans le même lieu
        doivent être séparées d'au moins {temps_inter_quêtes}."""
        q2_début = q2.début
        q2_fin = q2.fin
        if self.lieu and q2.lieu and q2.lieu != self.lieu:
            q2_début = q2_début - timedelta(minutes=temps_inter_quêtes)
            q2_fin = q2_fin + timedelta(minutes=temps_inter_quêtes)
        return not (self.fin <= q2_début or self.début >= q2_fin)

    def en_même_temps(self) -> filter[Quête]:
        """Return la liste de toutes les quêtes chevauchant celle-ci. Cette liste
        inclue la quête courante."""
        return filter(self.chevauche, Quête.toutes)


class Bénévole:
    """Classe permettant de gérer les bénévoles et maintenant à jour une liste de tous les bénévoles

    Les bénévoles sont identifiés par leur surnom qui doit donc être unique."""

    tous: Dict[str, Bénévole] = {}

    def __init__(
        self,
        id,
        surnom,
        prénom,
        nom,
        heures_théoriques,
        indisponibilités,
        pref_horaires,
        sérénité,
    ):
        self.id: str = id
        self.surnom: str = surnom if surnom else prénom
        self.prénom: str = prénom
        self.nom: str = nom
        self.sérénité: bool = sérénité
        self.heures_théoriques: int = heures_théoriques
        self.score_types_de_quêtes: Dict[Type_de_quête, int] = {}
        self.binômes_interdits: List[Bénévole] = []
        self.lieux_interdits: List[Lieu] = []
        self.indisponibilités: List[time] = indisponibilités
        self.pref_horaires: Dict[time, int] = pref_horaires
        Bénévole.tous[self.id] = self

    def __hash__(self):
        return hash(self.id)

    def __cmp__(self, lautre):
        return (self.id > lautre.id) - (self.id < lautre.id)

    def __eq__(self, lautre):
        return self.__cmp__(lautre) == 0

    def __str__(self) -> str:
        return f"{self.surnom}"

    def equal(self, lautre):
        self.id == lautre.id

    # def appréciation_dune_quête(self, quête):
    #     return self.score_types_de_quêtes.get(quête.type, 0)

    def appréciation_du_planning(self, planning):
        for (b, q, n), _ in enumerate(filter(lambda q: q == 1, planning)):
            if self == Bénévole.tous[b]:
                print(q, n)
