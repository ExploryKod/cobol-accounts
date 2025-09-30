# Makefile pour le système bancaire COBOL
# Compilateur COBOL
CC = cobc

# Options de compilation
CFLAGS = -free -x -Wall

# Nom du programme exécutable
TARGET = AccountMainManagement

# Fichiers sources COBOL
SOURCES = AccountMainManagement.cob ConsultBalance.cob Transfer.cob Withdrawal.cob

# Règle par défaut
all: $(TARGET)

# Compilation du programme principal
$(TARGET): $(SOURCES)
	@echo "🔨 Compilation du système bancaire COBOL..."
	$(CC) $(CFLAGS) $(SOURCES)
	@echo "✅ Compilation terminée !"
	@echo "🚀 Exécutez avec: ./$(TARGET)"

# Compilation en mode debug
debug: CFLAGS += -g
debug: $(TARGET)
	@echo "🐛 Version debug compilée"

# Compilation optimisée
optimized: CFLAGS += -O2
optimized: $(TARGET)
	@echo "⚡ Version optimisée compilée"

# Exécution du programme
run: $(TARGET)
	@echo "🏦 Lancement du système bancaire..."
	./$(TARGET)

# Nettoyage des fichiers générés
clean:
	@echo "🧹 Nettoyage des fichiers..."
	rm -f $(TARGET)
	rm -f *.o
	rm -f *.so
	@echo "✅ Nettoyage terminé"

# Installation (copie dans /usr/local/bin)
install: $(TARGET)
	@echo "📦 Installation du programme..."
	sudo cp $(TARGET) /usr/local/bin/
	@echo "✅ Installation terminée !"
	@echo "💡 Vous pouvez maintenant exécuter '$(TARGET)' depuis n'importe où"

# Désinstallation
uninstall:
	@echo "🗑️  Désinstallation du programme..."
	sudo rm -f /usr/local/bin/$(TARGET)
	@echo "✅ Désinstallation terminée"

# Vérification de la syntaxe sans compilation
check:
	@echo "🔍 Vérification de la syntaxe COBOL..."
	$(CC) -free -fsyntax-only $(SOURCES)
	@echo "✅ Syntaxe correcte !"

# Affichage de l'aide
help:
	@echo "📋 Commandes disponibles :"
	@echo "  make          - Compile le programme"
	@echo "  make debug    - Compile en mode debug"
	@echo "  make optimized- Compile en mode optimisé"
	@echo "  make run      - Compile et exécute"
	@echo "  make clean    - Nettoie les fichiers générés"
	@echo "  make install  - Installe le programme"
	@echo "  make uninstall- Désinstalle le programme"
	@echo "  make check    - Vérifie la syntaxe"
	@echo "  make help     - Affiche cette aide"

# Règles qui ne correspondent pas à des fichiers
.PHONY: all debug optimized run clean install uninstall check help

# Informations sur le projet
info:
	@echo "🏦 Système de Gestion Bancaire COBOL"
	@echo "📁 Fichiers sources: $(SOURCES)"
	@echo "🎯 Cible: $(TARGET)"
	@echo "⚙️  Compilateur: $(CC)"
	@echo "🔧 Options: $(CFLAGS)"
