from __future__ import annotations  # allows class type usage in class decl
import csv, re, sys
from datetime import date, time, datetime, timedelta

from data_model import Bénévole, Lieu, Type_de_quête, Quête


def update_pref_horaire(heure_début, data, prefs, indisponibilités):
    heure_fin = heure_début + 1
    if heure_début == 23:
        heure_fin = 0
    pref = data[f"{heure_début:0=2d}H{heure_fin:0=2d}H"]
    val = 0
    if pref == "indisponible":
        indisponibilités.append(time(hour=heure_début))
        return
    if pref.startswith("Dispo sous"):
        val = -1
    if pref.startswith("Dispo et horaire"):
        val = 1
    prefs[time(hour=heure_début)] = val


""" Importation des données """
csv_lieux = sys.argv[1]  # That's not very flexible...
csv_types_de_quête = sys.argv[2]
csv_bénévoles = sys.argv[3]
csv_quêtes = sys.argv[4]

encoding = "utf-8-sig"
dialect = csv.excel  # Dialecte par default
with open(csv_bénévoles, newline="", encoding=encoding) as csvfile:
    # We expect the dialect to be the same for every input csv
    dialect = csv.Sniffer().sniff(csvfile.read(1024))

with open(csv_lieux, newline="", encoding=encoding) as csvfile:
    reader = csv.DictReader(csvfile, dialect=dialect)
    for row in reader:
        Lieu(row["Name"])

with open(csv_types_de_quête, newline="", encoding=encoding) as csvfile:
    reader = csv.DictReader(csvfile, dialect=dialect)
    for row in reader:
        sécable = row["Découpable ? "] == "oui"
        Type_de_quête(row["Name"], sécable)

with open(csv_bénévoles, newline="", encoding=encoding) as csvfile:
    reader = csv.DictReader(csvfile, dialect=dialect)
    for row in reader:
        if row["Prénom"]:
            indisponibilités = []
            pref_horaires = {}
            update_pref_horaire(0, row, pref_horaires, indisponibilités)
            for i in range(9, 23 + 1):
                update_pref_horaire(i, row, pref_horaires, indisponibilités)
            Bénévole(
                row["Name"],
                row["Prénom"],
                row["Full Name"],
                int(row["heures théoriques par jour"]),
                indisponibilités,
                pref_horaires,
            )


def parse_horaires(horaire: str):
    horaire_groups = re.match(r"(.*) \(.*\) → (.*) \(.*\)", horaire)
    if horaire_groups:
        début = datetime.strptime(horaire_groups.group(1), "%d/%m/%Y %H:%M")
        fin = datetime.strptime(horaire_groups.group(2), "%d/%m/%Y %H:%M")
        return début, fin
    else:
        horaire_groups = re.match(r"(.*) \(.*\) → (.*):(.*)", horaire)
        début = datetime.strptime(horaire_groups.group(1), "%d/%m/%Y %H:%M")
        heure_fin = time(int(horaire_groups.group(2)), int(horaire_groups.group(3)))
        fin = datetime.combine(début, heure_fin)
        return début, fin


class ParseException(Exception):
    pass


with open(csv_quêtes, newline="", encoding=encoding) as csvfile:
    reader = csv.DictReader(csvfile, dialect=dialect)
    for row in reader:
        try:
            try:
                place = re.match(r"(.*) \(http", row["Place"]).group(1)
            except:
                raise ParseException("place", row["Place"])
            try:
                types_de_quête = row["Type de Quete"].split(", ")
                types_de_quête = list(
                    map(lambda t: re.match(r"(.*) \(http", t).group(1), types_de_quête)
                )
            except:
                raise ParseException("type", row["Type de Quete"])
            try:
                début, fin = parse_horaires(row["Horaire"])
            except ValueError as e:
                raise ParseException(f"horaire ({e.args[0]})", row["Horaire"])
            type_de_quête = list(map(lambda t: Type_de_quête.tous[t], types_de_quête))
            if all(t.sécable for t in type_de_quête):
                fin_acc = début
                while fin_acc < fin:
                    début_acc = fin_acc
                    # fin_acc = min(fin_acc + timedelta(minutes=15), fin)
                    fin_acc = min(fin_acc + timedelta(minutes=120), fin)
                    Quête(
                        row["Name"],
                        type_de_quête,
                        Lieu.tous[place],
                        int(row["Needed"]),
                        début_acc,
                        fin_acc,
                    )
            else:
                Quête(
                    row["Name"],
                    type_de_quête,
                    Lieu.tous[place],
                    int(row["Needed"]),
                    début,
                    fin,
                )

        except ParseException as e:
            print(f"Cannot parse {e.args[0]}: '{e.args[1]}'")
