# 🏦 Système de Gestion Bancaire COBOL

Un système bancaire simple développé en COBOL avec interface terminal, permettant la consultation de solde, les virements entre comptes et les retraits.

## 📋 Fonctionnalités

- **Consultation de solde** : Affiche le solde du compte principal
- **Virement** : Transfert d'argent entre deux comptes (compte 23 → compte 45)
- **Retrait** : Retrait d'argent du compte principal
- **Gestion d'erreurs** : Vérification des fonds suffisants
- **Interface menu** : Navigation simple par numéros

## 🏗️ Architecture

### Fichiers du projet
- `AccountMainManagement.cob` - Programme principal et menu
- `ConsultBalance.cob` - Module de consultation de solde
- `Transfer.cob` - Module de virement entre comptes
- `Withdrawal.cob` - Module de retrait
- `index.html` - Page de présentation web

### Comptes par défaut
- **Compte 1** : ID `00023`, Solde initial `02000.00`
- **Compte 2** : ID `00045`, Solde initial `07000.00`

## 🛠️ Installation

### Prérequis
- **GNU COBOL** (OpenCOBOL/GnuCOBOL)
- **Make** (optionnel, pour automatiser la compilation)
- **Terminal** compatible

### Installation de GNU COBOL

#### Ubuntu/Debian
```bash
sudo apt update
sudo apt install gnucobol4
```

#### CentOS/RHEL/Fedora
```bash
# CentOS/RHEL
sudo yum install gnucobol

# Fedora
sudo dnf install gnucobol
```

#### macOS (avec Homebrew)
```bash
brew install gnu-cobol
```

#### Compilation depuis les sources
```bash
# Télécharger GnuCOBOL
wget https://sourceforge.net/projects/gnucobol/files/gnucobol/3.2/gnucobol-3.2.tar.gz
tar -xzf gnucobol-3.2.tar.gz
cd gnucobol-3.2

# Compiler et installer
./configure
make
sudo make install
```

## 🚀 Compilation et Exécution

### Compilation manuelle
```bash
# Compiler tous les modules
cobc -free -x AccountMainManagement.cob ConsultBalance.cob Transfer.cob Withdrawal.cob

# Exécuter le programme
./AccountMainManagement
```

### Compilation avec Makefile (optionnel)
```bash
# Créer un Makefile
cat > Makefile << 'EOF'
CC = cobc
CFLAGS = -free -x
TARGET = AccountMainManagement
SOURCES = AccountMainManagement.cob ConsultBalance.cob Transfer.cob Withdrawal.cob

$(TARGET): $(SOURCES)
	$(CC) $(CFLAGS) $(SOURCES)

clean:
	rm -f $(TARGET)

.PHONY: clean
EOF

# Compiler
make

# Exécuter
./AccountMainManagement

# Nettoyer
make clean
```

## 🎮 Utilisation

### Lancement du programme
```bash
./AccountMainManagement
```

### Menu interactif
```
Menu:
1. Consult Balance
2. Virement du compte n°00023 au compte n°00045
3. Withdrawal
4. Quitter
```

### Exemples d'utilisation

#### 1. Consulter le solde
- Choisir `1`
- Le programme affiche le solde du compte 23

#### 2. Effectuer un virement
- Choisir `2`
- Saisir le montant à transférer
- Le programme vérifie les fonds et effectue le transfert

#### 3. Effectuer un retrait
- Choisir `3`
- Saisir le montant à retirer
- Le programme vérifie les fonds et effectue le retrait

#### 4. Quitter
- Choisir `4`
- Le programme s'arrête

## 🔧 Configuration

### Modifier les comptes par défaut
Éditez `AccountMainManagement.cob` lignes 9-12 :
```cobol
01 ACCOUNT-1-ID         PIC 9(5) VALUE 23.      # ID du compte 1
01 ACCOUNT-2-ID         PIC 9(5) VALUE 45.      # ID du compte 2
01 ACCOUNT-1-BALANCE    PIC 9(5)V99 VALUE 2000.00.  # Solde compte 1
01 ACCOUNT-2-BALANCE    PIC 9(5)V99 VALUE 7000.00.  # Solde compte 2
```

### Codes d'erreur
- `0` : Opération réussie
- `1` : Fonds insuffisants
- `2` : Erreur sur le compte destinataire

## 🌐 Page Web de Présentation

Le projet inclut une page HTML moderne pour présenter le code :
```bash
# Lancer un serveur local
python3 -m http.server 8000

# Ouvrir dans le navigateur
# http://localhost:8000
```

## 📁 Structure du Projet
```
cobol-accounts/
├── AccountMainManagement.cob    # Programme principal
├── ConsultBalance.cob           # Module consultation
├── Transfer.cob                 # Module virement
├── Withdrawal.cob               # Module retrait
├── index.html                   # Page de présentation
├── README.md                    # Cette documentation
└── Makefile                     # Compilation automatique (optionnel)
```

## 🐛 Dépannage

### Erreur de compilation
```bash
# Vérifier l'installation de GnuCOBOL
cobc --version

# Compiler avec plus de détails
cobc -free -x -Wall AccountMainManagement.cob ConsultBalance.cob Transfer.cob Withdrawal.cob
```

### Erreur d'exécution
```bash
# Vérifier les permissions
chmod +x AccountMainManagement

# Exécuter avec debug
cobc -free -g -x AccountMainManagement.cob ConsultBalance.cob Transfer.cob Withdrawal.cob
gdb ./AccountMainManagement
```

## 📝 Notes Techniques

- **Format des montants** : `PIC 9(5)V99` (5 chiffres avant la virgule, 2 après)
- **Affichage** : Les montants s'affichent avec des zéros de remplissage (ex: `02000.00`)
- **Gestion mémoire** : Utilisation de la section WORKING-STORAGE et LINKAGE
- **Modularité** : Chaque fonction est dans un module séparé

## 🤝 Contribution

1. Fork le projet
2. Créer une branche feature (`git checkout -b feature/nouvelle-fonctionnalite`)
3. Commit les changements (`git commit -am 'Ajouter nouvelle fonctionnalité'`)
4. Push vers la branche (`git push origin feature/nouvelle-fonctionnalite`)
5. Créer une Pull Request

## 📄 Licence

Ce projet est sous licence MIT. Voir le fichier LICENSE pour plus de détails.

## 👨‍💻 Auteur

Développé dans le cadre d'un projet d'apprentissage du langage COBOL.

---

**Note** : Ce système est à des fins éducatives. Pour un usage bancaire réel, des mesures de sécurité supplémentaires seraient nécessaires.
