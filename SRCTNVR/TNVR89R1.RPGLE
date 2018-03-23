000100010205     H DECEDIT('0,') DATEDIT(*DMY.)
000200020807     FTISAV     IF   E             DISK
000300020807     FECCET30C  IF   E           K DISK
000400010205     D*------------------------------------------------------------------------*
000500020807     D* DATA STRUCTURE
000600010205     D*------------------------------------------------------------------------*
000700020807     D*---
000800020807     D* DS  CONTROLLO DATA (8)
000900020807     D*---
001000020807     D WLBDA8          DS                  INZ
001100020807     D  G08DAT                 1      8  0
001200020807     D  G08INV                 9     16  0
001300020807     D  G08ERR                17     17
001400020807     D  G08TGI                18     22  0
001500020807     D*---
001600010205     D* DS ESTERNA PER GUFO
001700020807     D*---
001800010205     D DS£ECO        E DS
001900020807     D*---
002000010205     D* DS VALORIZZAZIONE IMMAGINE ERRORE DA TITAS
002100020807     D*---
002200010205     D TRECDS        E DS
002300010205     D  D$PDS1                 1    250
002400010205     D  D$PDS2               251    500
002500010205     D  D$DDS1               501    750
002600010205     D  D$DDS2               751   1000
002700010205     C*------------------------------------------------------------------------*
002800010205     C* MAIN LINES
002900010205     C*------------------------------------------------------------------------*
003000010205     C***
003100020807     C* LEGGE TISAV
003200010205     C***
003300020807     C                   READ      TISAV                                  99
003400010205DO  1C     *IN99         DOWEQ     *OFF
003500010205     C*
003600020807     C* CONTROLLA SE BOLLA DA CONSIDERARE
003700020807     C                   EXSR      CHKREC
003800010205IF  2C     WRECOK        IFEQ      'S'
003900020807     C***
004000020807     C* RILANCIA IL CONTO ECONOMICO
004100020807     C***
004200010205     C                   EXSR      APDTAQ
004300010205     C                   EXSR      CDDTAQ
004400010205     C                   EXSR      GUFO
004500020807E   2C                   ENDIF
004600010205     C*
004700010205     C* LETTURA SUCCESSIVA ERRORI DA RILANCIARE
004800020807     C                   READ      TISAV                                  99
004900010205E   1C                   ENDDO
005000010205     C*
005100010205     C                   SETON                                        LR
005200010205     C*------------------------------------------------------------------------*
005300010205     C* GUFO - LANCIA IL PGM CHE RIMETTE LA BOLLA IL CODA PER ESSERE ELABORATA
005400010205     C*------------------------------------------------------------------------*
005500010205     C     GUFO          BEGSR
005600020807     C*
005700020807     C* REPERISCE DATA E ORA CORRENTE
005800020807     C                   TIME                    N14              14 0
005900020807     C                   MOVEL     N14           ORACOR            6 0
006000020807     C                   MOVE      N14           N8                8 0
006100020807     C                   Z-ADD     N8            G08DAT
006200020807     C                   Z-ADD     *ZEROS        G08INV
006300020807     C                   MOVEL     '0'           G08ERR
006400020807     C                   CALL      'XSRDA8'
006500020807     C                   PARM                    WLBDA8
006600020807     C                   Z-ADD     G08INV        DATCOR            8 0
006700010205     C*
006800020807     C* IMPOSTA LA DS DI PASSAGGIO
006900020807     C                   MOVEL     'I'           D£GSTS
007000020807     C                   Z-ADD     DATCOR        D£GDAT
007100020807     C                   Z-ADD     ORACOR        D£GTIM
007200020807     C                   MOVEL     'EDPRN'       D£GUSR
007300020807     C                   MOVEL     'TREC89R1'    D£GPGF
007400020807     C                   MOVEL     'TITAS10F'    D£GFIL
007500020807     C                   MOVEL     'TITAS10F'    D£GMBR
007600020807     C                   MOVEL     'GAITRAGRU'   D£GLIB
007700020807     C                   MOVEL     ' '           D£GPDE
007800020731     C                   MOVEL     'I'           D£GTJB
007900010205     C                   MOVEL     *BLANKS       D£GXXX
008000020807     C                   MOVEL     D$PDS1        D£PRE1
008100020807     C                   MOVEL     D$PDS2        D£PRE2
008200020807     C                   MOVEL     D$DDS1        D£DRE1
008300020807     C                   MOVEL     D$DDS2        D£DRE2
008400010205     C*
008500020807     C* LANCIA IL PROGRAMMA DI ACCODAMENTO
008600020619     C                   CALL      'TRECD2R'
008700010205     C                   PARM                    DS£ECO
008800010205     C*
008900010205     C                   ENDSR
009000010205     C*------------------------------------------------------------------------*
009100020807     C* CHKREC - CONTROLLA VALIDITA' RECORD
009200010205     C*------------------------------------------------------------------------*
009300020807     C     CHKREC        BEGSR
009400010205     C*
009500010205     C                   MOVEL     'S'           WRECOK            1
009600020807     C*
009700020807     C* SE PER LA BOLLA ESISTE GIA' LA COMPETENZA, NON LANCIA IL C/E
009800020807     C     KEYCET        CHAIN     ECCET30C                           98
009900020807IF  1C     *IN98         IFEQ      *OFF
010000020807     C                   MOVEL     'N'           WRECOK
010100020807E   1C                   ENDIF
010200010205     C*
010300010205     C                   ENDSR
010400010205     C*------------------------------------------------------------------------*
010500010205     C*  *INZSR - ROUTINE INIZIALE
010600010205     C*------------------------------------------------------------------------*
010700010205     C     *INZSR        BEGSR
010800010205     C*--------------------
010900010205     C* CHIAVI DI LETTURA
011000010205     C*--------------------
011100020807     C* ECCET30C - COMPLETA
011200020807     C     KEYCET        KLIST
011300020807     C                   KFLD                    TASAAS
011400020807     C                   KFLD                    TASLNP
011500020807     C                   KFLD                    TASNRS
011600020807     C                   KFLD                    TASNSP
011700020807     C                   KFLD                    TASTBL
011800010205     C*
011900010205     C                   ENDSR
012000010205     C*------------------------------------------------------------------------*
012100010205     C* ROUTINE GENERALIZZATA PASSAGGIO IMMAGINI BOLLA DI SEDE
012200010205     C*------------------------------------------------------------------------*
012300010205      /COPY GAITRASRC/SRCTREC,TRECDT
