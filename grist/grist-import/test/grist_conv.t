  $ cat >initial <<'EOF'
  > {
  >   "id": 1,
  >   "Slug": "🧹",
  >   "Nom": "Clean",
  >   "Fiche_de_poste": "",
  >   "Impose": "Non",
  >   "Specialiste_requis": false,
  >   "Decoupable": true,
  >   "Lieu_par_defaut": 1
  > }

  $ cat initial | ./grist_conv.exe task_type | jq | tee recoded
  {
    "id": 1,
    "Slug": "🧹",
    "Nom": "Clean",
    "Fiche_de_poste": "",
    "Impose": "Non",
    "Specialiste_requis": false,
    "Decoupable": true,
    "Lieu_par_defaut": 1
  }

  $ diff initial recoded

  $ cat >initial <<'EOF'
  > {
  >   "id": 1,
  >   "Slug": "🌍",
  >   "Nom": "Site",
  >   "Description": ""
  > }

  $ cat initial | ./grist_conv.exe lieu | jq | tee recoded
  {
    "id": 1,
    "Slug": "🌍",
    "Nom": "Site",
    "Description": ""
  }

  $ diff initial recoded


  $ cat >initial <<'EOF'
  > {
  >   "id": 1,
  >   "Pseudo": "f",
  >   "Nom": "",
  >   "Prenom": "Pouet2",
  >   "Nb_heures": 4,
  >   "Langues": [
  >     "L",
  >     "Français",
  >     "Anglais"
  >   ],
  >   "Telephone": "",
  >   "Email": "",
  >   "Specialites": null,
  >   "Taches_interdites": null,
  >   "Lieux_interdits": null,
  >   "Amis": [
  >     "L",
  >     1
  >   ],
  >   "Ennemis": null,
  >   "Date_d_arrivee": null,
  >   "Date_de_depart": null,
  >   "Indisponibilites_quotidiennes": [
  >     "L",
  >     8,
  >     9,
  >     11
  >   ],
  >   "Horaires_preferes": null,
  >   "Horaires_contraints": null,
  >   "Indisponibilites_ponctuelles": null
  > }
  > EOF


  $ cat initial | ./grist_conv.exe benevole | jq >recoded

  $ diff initial recoded


  $ cat >initial <<'EOF'
  > {
  >   "id": 1,
  >   "manualSort": 1,
  >   "Nom": "Clean 1",
  >   "Type": 1,
  >   "Lieu": 1,
  >   "Recurrence": "Ponctuelle",
  >   "Jours": null,
  >   "Date_et_heure_de_debut": 1775752200,
  >   "benevoles": 2,
  >   "Duree_heures_": 2,
  >   "Date_et_heure_de_fin": 1775759400,
  >   "ModCount": 6,
  >   "Benevoles_assignes": [
  >     "L",
  >     1
  >   ],
  >   "gristHelper_Display2": "🧹 Clean",
  >   "gristHelper_Display": "🌍 Site",
  >   "gristHelper_Display4": [
  >     "L",
  >     "jjj"
  >   ],
  >   "Display": "🧹 Clean"
  > }
  > EOF

  $ cat initial | ./grist_conv.exe quete | jq >recoded
