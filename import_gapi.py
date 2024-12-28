from import_gapi_req import get
from data_model import Bénévole, Spectacle, Lieu, Type_de_quête, Quête

from xlrd import xldate_as_datetime
from datetime import date, time, datetime, timedelta


def load_lieux(data):
    lieux = data["lieux"]
    for p in lieux:
        id = p["id"]
        nom = p["nom"]
        Lieu(id, nom)


def load_types(data):
    types = data["types_de_quêtes"]
    for t in types:
        id = t["id"]
        nom = t["nom"]
        sécable = False
        Type_de_quête(id, nom, sécable)


def to_datetime(date):
    if date:
        return xldate_as_datetime(date, 0)
    return None


def load_bénévoles(data):
    bénévoles = data["bénévoles"]
    for b in bénévoles:
        id = b["id"]
        surnom = b["pseudo"]
        prénom = ""
        nom = ""
        heures_théoriques = b["heures_théoriques"]
        indisponibilités = []  # TODO
        pref_horaires = {}  # TODO
        sérénité = True  # TODO avec les "quêtes interdites"
        binômes_préférés = []  # TODO
        binômes_interdits = []  # TODO
        types_de_quête_interdits = []  # TODO
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
            date_arrivée,
            date_départ,
        )


def main():
    data = get()
    if not (data is None):
        print(data)
        load_lieux(data)
        load_types(data)
        load_bénévoles(data)


if __name__ == "__main__":
    main()
