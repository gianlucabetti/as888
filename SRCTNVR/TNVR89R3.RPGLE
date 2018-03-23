000100010205     H DECEDIT('0,') DATEDIT(*DMY.)
000200020807     FECERR00F  IF   E             DISK
000300020807     FTITAS30C  IF   E           K DISK
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
002700020807     ITITAS000      01
002800020807     ITITAS010      02
002900010205     C*------------------------------------------------------------------------*
003000010205     C* MAIN LINES
003100010205     C*------------------------------------------------------------------------*
003200010205     C***
003300020807     C* LEGGE TISAV
003400010205     C***
003500020807     C                   READ      ECERR00F                               99
003600010205DO  1C     *IN99         DOWEQ     *OFF
003700010205     C*
003800020807     C* CONTROLLA SE BOLLA DA CONSIDERARE
003900020807     C                   EXSR      CHKREC
004000010205IF  2C     WRECOK        IFEQ      'S'
004100020807     C***
004200020807     C* LEGGE LA BOLLA
004300020807     C***
004400020807     C                   SETOFF                                       0102
004500020807     C     KEYERR        CHAIN     TITAS30C                           98
004600020807if  3c     *In98         IFEQ      *OFF
004700020807     C***
004800020807     C* RILANCIA IL CONTO ECONOMICO
004900020807     C***
005000010205     C                   EXSR      APDTAQ
005100010205     C                   EXSR      CDDTAQ
005200010205     C                   EXSR      GUFO
005300020807E   3C                   ENDIF
005400020807E   2C                   ENDIF
005500010205     C*
005600010205     C* LETTURA SUCCESSIVA ERRORI DA RILANCIARE
005700020807     C                   READ      ECERR00F                               99
005800010205E   1C                   ENDDO
005900010205     C*
006000010205     C                   SETON                                        LR
006100010205     C*------------------------------------------------------------------------*
006200010205     C* GUFO - LANCIA IL PGM CHE RIMETTE LA BOLLA IL CODA PER ESSERE ELABORATA
006300010205     C*------------------------------------------------------------------------*
006400010205     C     GUFO          BEGSR
006500020807     C*
006600020807     C* REPERISCE DATA E ORA CORRENTE
006700020807     C                   TIME                    N14              14 0
006800020807     C                   MOVEL     N14           ORACOR            6 0
006900020807     C                   MOVE      N14           N8                8 0
007000020807     C                   Z-ADD     N8            G08DAT
007100020807     C                   Z-ADD     *ZEROS        G08INV
007200020807     C                   MOVEL     '0'           G08ERR
007300020807     C                   CALL      'XSRDA8'
007400020807     C                   PARM                    WLBDA8
007500020807     C                   Z-ADD     G08INV        DATCOR            8 0
007600010205     C*
007700020807     C* IMPOSTA LA DS DI PASSAGGIO
007800020807     C                   MOVEL     'I'           D£GSTS
007900020807     C                   Z-ADD     DATCOR        D£GDAT
008000020807     C                   Z-ADD     ORACOR        D£GTIM
008100020807     C                   MOVEL     'EDPRN'       D£GUSR
008200020807     C                   MOVEL     'TNVR89R3'    D£GPGF
008300020807     C   01              MOVEL     'TITAS00F'    D£GFIL
008400020807     C   01              MOVEL     'TITAS00F'    D£GMBR
008500020807     C   02              MOVEL     'TITAS10F'    D£GFIL
008600020807     C   02              MOVEL     'TITAS10F'    D£GMBR
008700020807     C                   MOVEL     'GAITRAGRU'   D£GLIB
008800020807     C                   MOVEL     ' '           D£GPDE
008900020731     C                   MOVEL     'I'           D£GTJB
009000010205     C                   MOVEL     *BLANKS       D£GXXX
009100020807     C                   MOVEL     D$PDS1        D£PRE1
009200020807     C                   MOVEL     D$PDS2        D£PRE2
009300020807     C                   MOVEL     D$DDS1        D£DRE1
009400020807     C                   MOVEL     D$DDS2        D£DRE2
009500010205     C*
009600020807     C* LANCIA IL PROGRAMMA DI ACCODAMENTO
009700020619     C                   CALL      'TRECD2R'
009800010205     C                   PARM                    DS£ECO
009900010205     C*
010000010205     C                   ENDSR
010100010205     C*------------------------------------------------------------------------*
010200020807     C* CHKREC - CONTROLLA VALIDITA' RECORD
010300010205     C*------------------------------------------------------------------------*
010400020807     C     CHKREC        BEGSR
010500010205     C*
010600010205     C                   MOVEL     'S'           WRECOK            1
010700020807     C*
010800020807IF  1C     ERRCER        IFNE      'NCET'
010900020807     C                   MOVEL     'N'           WRECOK
011000020807E   1C                   ENDIF
011100010205     C*
011200010205     C                   ENDSR
011300010205     C*------------------------------------------------------------------------*
011400010205     C*  *INZSR - ROUTINE INIZIALE
011500010205     C*------------------------------------------------------------------------*
011600010205     C     *INZSR        BEGSR
011700020807     C     KEYERR        KLIST
011800020807     C                   KFLD                    ERRAAS
011900020807     C                   KFLD                    ERRLNP
012000020807     C                   KFLD                    ERRNRS
012100020807     C                   KFLD                    ERRNSP
012200020807     C                   KFLD                    ERRTBL
012300010205     C*
012400010205     C                   ENDSR
012500010205     C*------------------------------------------------------------------------*
012600010205     C* ROUTINE GENERALIZZATA PASSAGGIO IMMAGINI BOLLA DI SEDE
012700010205     C*------------------------------------------------------------------------*
012800010205      /COPY GAITRASRC/SRCTREC,TRECDT
