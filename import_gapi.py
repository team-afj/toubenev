from functools import reduce
from import_gapi_req import get
from data_model import Bénévole, Spectacle, Lieu, Type_de_quête, Quête

from xlrd import xldate_as_datetime
from datetime import date, time, datetime, timedelta


def to_bool(s):
    return True if s == "TRUE" else False


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
        sécable = to_bool(t["découpable"])
        spécialiste_only = to_bool(t["only_spe"])
        Type_de_quête(id, nom, sécable, spécialiste_only)


def to_datetime(date):
    if date:
        return xldate_as_datetime(date, 0)
    return None


def split(s):
    return [] if s == None or s == "" else s.split(",")


def load_bénévoles(data):
    bénévoles = data["bénévoles"]
    for b in bénévoles:

        def times_of_hours_list(horaires: str):
            # 08h - 09h, 09h - 10h, 00h - 01h
            # => [8, 9, 0]
            if not horaires.strip():
                return []
            else:
                return list(
                    map(
                        lambda s: time(hour=int(s.strip()[:2])),
                        horaires.split(sep=", "),
                    )
                )

        def make_indisponibilités():
            indisponibilités = times_of_hours_list(b["h_indispos"])
            return indisponibilités

        def make_pref_horaires():
            acc = {}
            for t in times_of_hours_list(b["h_contraints"]):
                acc[t] = -1
            for t in times_of_hours_list(b["h_prefs"]):
                acc[t] = 1
            return acc

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


def with_default(s, f, default):
    try:
        return f(s)
    except:
        return default


def load_quêtes(data):
    quêtes = data["quêtes"]
    # TODO GROUPES
    for q in quêtes:
        id = str(q["id"])
        nom = str(q["nom"])
        types = list(map(Type_de_quête.tous.get, split(q["types"])))
        lieu = Lieu.tous[str(q["lieu"])]
        spectacle = None
        nombre_bénévoles = with_default(q["nombre_bénévoles"], int, 2)
        début = to_datetime(q["début"])
        fin = to_datetime(q["fin"])
        bénévoles = list(map(Bénévole.tous.get, split(q["fixés"])))
        try:
            Quête(
                id, nom, types, lieu, spectacle, nombre_bénévoles, début, fin, bénévoles
            )
        except:
            return


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
