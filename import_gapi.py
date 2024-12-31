from functools import reduce
from import_gapi_req import get
from data_model import Bénévole, Spectacle, Lieu, Type_de_quête, Quête

from xlrd import xldate_as_datetime
from datetime import date, time, datetime, timedelta


def load_lieux(data):
    lieux = data["lieux"]
    for p in lieux:
        id = str(p["id"])
        nom = p["nom"]
        Lieu(id, nom)


def load_types(data):
    types = data["types_de_quêtes"]
    for t in types:
        id = str(t["id"])
        nom = t["nom"]
        sécable = False
        Type_de_quête(id, nom, sécable)


def to_datetime(date):
    if date:
        return xldate_as_datetime(date, 0)
    return None


def split(s):
    return [] if s == None or s == "" else s.split(",")


def load_bénévoles(data):
    bénévoles = data["bénévoles"]
    for b in bénévoles:

        def pref_of_hour(hour):
            def score_of(s):
                if s == "Contraint":
                    return -1
                if s == "Préféré":
                    return 1
                return 0

            if 0 <= hour and hour <= 5:
                return score_of(b["nuit"])

            if 6 <= hour and hour <= 11:
                return score_of(b["matin"])

            if 12 <= hour and hour <= 17:
                return score_of(b["aprem"])

            if 18 <= hour and hour <= 23:
                return score_of(b["soir"])

        def make_pref_horaires():
            acc = {}
            for h in range(0, 23 + 1):
                acc[time(hour=h)] = pref_of_hour(h)
            return acc

        def make_indisponibilités():
            indisponibilités = []
            if b["nuit"] == "Indisponible":
                for h in range(0, 6):
                    indisponibilités.append(time(hour=h))
            if b["matin"] == "Indisponible":
                for h in range(6, 12):
                    indisponibilités.append(time(hour=h))
            if b["aprem"] == "Indisponible":
                for h in range(12, 18):
                    indisponibilités.append(time(hour=h))
            if b["soir"] == "Indisponible":
                for h in range(12, 24):
                    indisponibilités.append(time(hour=h))
            return indisponibilités

        id = str(b["id"])
        surnom = b["pseudo"]
        prénom = ""
        nom = ""
        heures_théoriques = b["heures_théoriques"]
        indisponibilités = make_indisponibilités()
        pref_horaires = make_pref_horaires()
        sérénité = True  # TODO avec les "quêtes interdites"
        binômes_préférés = split(b["amis"])
        binômes_interdits = split(b["ennemis"])
        types_de_quête_interdits = split(b["quêtes_interdites"])
        spécialités = split(b["spécialités"])
        date_arrivée = to_datetime(b["arrivée"])
        date_départ = to_datetime(b["départ"])
        Bénévole(
            id,
            surnom,
            prénom,
            nom,
            heures_théoriques,
            indisponibilités,
            pref_horaires,
            sérénité,
            binômes_préférés,
            binômes_interdits,
            types_de_quête_interdits,
            spécialités,
            date_arrivée,
            date_départ,
        )


def load_quêtes(data):
    quêtes = data["quêtes"]
    # TODO GROUPES
    for q in quêtes:
        id = str(q["id"])
        nom = str(q["nom"])
        types = list(map(Type_de_quête.tous.get, split(q["types"])))
        lieu = Lieu.tous[str(q["lieu"])]
        spectacle = None
        nombre_bénévoles = int(q["nombre_bénévoles"])
        début = to_datetime(q["début"])
        fin = to_datetime(q["fin"])
        bénévoles = list(map(Bénévole.tous.get, split(q["fixés"])))
        Quête(id, nom, types, lieu, spectacle, nombre_bénévoles, début, fin, bénévoles)


def main():
    data = get()
    if not (data is None):
        load_lieux(data)
        load_types(data)
        load_bénévoles(data)
        load_quêtes(data)
    Bénévole.strengthen()


if __name__ == "__main__":
    main()
