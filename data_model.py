from __future__ import annotations  # allows class type usage in class decl
from typing import List, Dict, Optional
from datetime import date, time, datetime, timedelta
import string

""" Paramètres """

temps_inter_quêtes = 15  # minutes

""" Sales types """


def resolve(src, l):
    """
    `resolve(src, l)` replaces all strings in `l` by the value indexed by
    that string in `src`
    """
    for i, elt in enumerate(l):
        if isinstance(elt, str):
            l[i] = src.get(elt)


class Spectacle:
    tous: Dict[str, Spectacle] = {}

    def __init__(self, id, nom):
        self.id: str = id
        self.nom = nom
        Spectacle.tous[self.id] = self

    def __str__(self) -> str:
        return f"{self.id}:{self.nom}"

    def __hash__(self):
        return hash(self.id)

    def __cmp__(self, lautre):
        return (self.id > lautre.id) - (self.id < lautre.id)

    def __eq__(self, lautre):
        return self.__cmp__(lautre) == 0


class Lieu:
    tous: Dict[str, Lieu] = {}

    def __init__(self, id, nom):
        self.id: str = id
        self.nom = nom
        Lieu.tous[self.id] = self

    def __str__(self) -> str:
        return f"{self.id}:{self.nom}"

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

    def __str__(self) -> str:
        return f"{self.id}:{self.nom}"

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
        if self.début.time() < time(hour=5):
            # Day starts at 4 am
            date_début = date_début - timedelta(days=1)
        self.jour: date = date_début

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
    """Classe permettant de gérer les bénévoles et maintenant à jour une liste
    de tous les bénévoles dans `Bénévole.tous`."""

    tous: Dict[str, Bénévole] = {}

    def strengthen():
        for _key, bénévole in Bénévole.tous.items():
            resolve(Bénévole.tous, bénévole.binômes_préférés)
            resolve(Bénévole.tous, bénévole.binômes_interdits)
            resolve(Type_de_quête.tous, bénévole.types_de_quête_interdits)
            resolve(Type_de_quête.tous, bénévole.spécialités)

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
        binômes_préférés=[],
        binômes_interdits=[],
        types_de_quête_interdits=[],
        spécialités=[],
        date_arrivée=None,
        date_départ=None,
    ):
        self.id: str = id
        self.surnom: str = surnom if surnom else prénom
        self.prénom: str = prénom
        self.nom: str = nom
        self.sérénité: bool = sérénité
        self.heures_théoriques: float = heures_théoriques
        self.score_types_de_quêtes: Dict[Type_de_quête, int] = {}
        self.binômes_préférés: List[str | Bénévole] = binômes_préférés
        self.binômes_interdits: List[Bénévole] = binômes_interdits
        self.lieux_interdits: List[Lieu] = []
        self.types_de_quête_interdits: List[str | Type_de_quête] = (
            types_de_quête_interdits
        )
        self.spécialités: List[str | Type_de_quête] = spécialités
        self.indisponibilités: List[time] = indisponibilités
        self.pref_horaires: Dict[time, int] = pref_horaires
        self.date_arrivée: Optional[datetime] = date_arrivée
        self.date_départ: Optional[datetime] = date_départ
        self.quêtes_assignées: List[Quête] = []
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

    def détails(self) -> str:
        amis = ", ".join("{}".format(k) for k in self.binômes_préférés)
        ennemis = ", ".join("{}".format(k) for k in self.binômes_interdits)
        pref_horaires = ", ".join(
            f"{h.hour}:{v}" for h, v in self.pref_horaires.items()
        )
        indisponibilités = ", ".join(f"{k.hour}" for k in self.indisponibilités)
        interdictions = ", ".join(f"{k}" for k in self.types_de_quête_interdits)
        spécialités = ", ".join(f"{k}" for k in self.spécialités)
        return f"{self.id}: {self.surnom} ({self.nom} {self.prénom}) ({self.heures_théoriques}h par jour)\nArrivée: {self.date_arrivée} Départ: {self.date_départ}\nAmis: {amis}\nEnnemis: {ennemis}\nPrefs: {pref_horaires}\nIndisponibilités: {indisponibilités}\nInterdictions: {interdictions}\nSpécialités: {spécialités}"

    # def appréciation_dune_quête(self, quête):
    #     return self.score_types_de_quêtes.get(quête.type, 0)

    def appréciation_du_planning(self, planning):
        for (b, q, n), _ in enumerate(filter(lambda q: q == 1, planning)):
            if self == Bénévole.tous[b]:
                print(q, n)

    """ Certains bénévole ont un ensemble de tâches précis à faire et ne doivent
    pas participer aux autres """

    def est_assigné(self, date):
        temps_assigné = 0
        for q in self.quêtes_assignées:
            if q.jour == date:
                temps_assigné += q.durée_minutes()
        return temps_assigné >= self.heures_théoriques * 60


def strengthen():
    """
    Dereferences all relations by looking in the tables. Must be called AFTER
    all data has been declared.
    """
    Bénévole.strengthen()
