Leitfaden für die Website:

Die Website ist wiefolgt aufgebaut: 

Auf der Ersten ordner Ebene befinden isch die .qmd Dokumente index.qmd, about.qmd project_overview.qmd und project_archive.qmd. Das sind die "statischen" Seiten der Website. about und index sind reine Textseiten während project_overview die 6 aktuellsten projekte präsentiert und auf project_archieves verweist in dem sämtliche Projekte aufgelistet sind.

Der Ordner projects enthält den "dynamischen" Teil der Website. Darin finden sich die Ordner für die einzelnen Projekte.

Um ein neues Projekt hinzuzufügen muss ein Ordner mit dem Namen des Projekts im Projects Ordner angelegt werden. Dieser muss folgende Bestandteile enthalten:

- die Datei metadata.xlsx die eine Tabelle mit den Spalten student_folder (anonymisierter Gruppenname), author1, author2, author3, und rank enthält.

- image.png für die Anzeige auf der Seite project overview

- die Datei meta.yml die die Daten zu den Projekten enthält:
    - "type": Typ des Projekts
    - "semester"
    - "categories": Liste mit Kategorien für das Projekt.
    - "description": Beschreibung und Motivation des Projekts.

- den ordner student_projects, dieser enthält für jede zu veröffentlichende Ausarbeitung einen Ordner der die HTML-Datei mit der Benennung Projekt_Projektname und den Screencast im .mp4 Format enthält.

Um die Website upzudaten müssen nur die Änderungen auf github gepusht werden. Das triggert einen GitHub Actions Workflow der automatisch die .qmd seiten generiert und auf GitHub pages veröffentlicht.

Dafür ist die unter .github/workflows/publish.yml liegende Datei verantwortlich. Diese Ruft unter anderm das Script generate_pages.R auf welches die .qmd Dateien erstellt. Dieses Script liegt im Ordner scripts. Dort findet man auch das Script delete_pages.R welches dazu genutzt werden kann um alle lokal gepeicherten "dynamisen" .qmd Datein zu löschen wenn man die Website bearbeiten und auch bestehende Seiten verändern will.