000100921020     H DECEDIT('0,') DATEDIT(*DMY.)
000200941118     H* TRTC22R  *---------------------------------------------------*
000300921020     H*     LANCIO PULIZIA STORICO MEMBRI                            *
000400921020     H*--------------------------------------------------------------*
000500020829     FTABEL00F  IF   E           K DISK
000600921020     D*------------------------------------------------------------------------*
000700050318     D**!!! CMD1            S             12    DIM(1) CTDATA PERRCD(1)
000800050318     D**!!! CMD2            S             16    DIM(1) CTDATA PERRCD(1)
000900921020     D*------------------------------------------------------------------------*
001000921020     D CNCR80        E DS
001100921020     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
001200921020     D PARAM           DS
001300921020     D  LIBR                   1     10
001400921020     D  FILE                  11     20
001500921020     D  DATA                  21     26  0
001600991221     D WLBDAT          DS                  INZ
001700991221     D  G02DAT                 1      8  0
001800991221     D  G02INV                 9     16  0
001900991221     D  G02ERR                17     17
002000991221     D  G02TGI                18     22  0
002100991221     D WGIDAT          DS                  INZ
002200991221     D  GIODAT                 1      8  0
002300991221     D  GIOINV                 9     16  0
002400991221     D  GIOTGI                17     21  0
002500921020     D DS3S          E DS
002600921020     D KPJBA         E DS
002700921019     I/SPACE 3
002800921019     C*---------------------------------------------------------------*
002900921019     C     *ENTRY        PLIST
003000921019     C                   PARM                    KPJBA
003100921020     C                   Z-ADD     1             CODUT             1 0
003200000000     C*---------------------------------------------------------------*
003300921015     C     KTB           KLIST
003400900607     C                   KFLD                    CODUT
003500921020     C                   KFLD                    COD               2
003600921020     C*---------------------------------------------------------------*
003700921020     C* PRELEVO SIMFEL PER TESTARE POI SE SONO IN SEDE O IN FILIALE
003800921020     C                   CALL      'X§PARUT'
003900921020     C                   PARM                    UTEDSE
004000921020     C                   MOVEL     REC80         CNCR80
004100000703     C                   SETOFF                                       60
004200921020     C* PRELEVO DATA DEL GIORNO E LA TRASFORMO IN GIORNI
004300921020     C                   Z-ADD     UDATE         G02DAT
004400921020     C                   MOVEL     *BLANKS       G02ERR
004500991221     C                   CALL      'XSRDA8'
004600921020     C                   PARM                    WLBDAT
004700921020     C*
004800921020     C                   MOVEL     '3S'          COD
004900921020     C     KTB           SETLL     TABEL
005000921020     C*
005100921020     C     *IN30         DOWEQ     '0'
005200921020     C     KTB           READE     TABEL                                  30
005300921020     C* VERIFICO SE X FILE IN TABELLA E' PREVISTO SCARICO DATI A CLIENTE
005400921020     C  N30              DO
005500921020     C*
005600921020     C                   MOVEL     TBLUNI        DS3S
005700000923     C     §3SFL1        IFEQ      'F'                                          SOLO FIL.
005800000923     C     SIMFEL        ANDNE     *ZEROS
005900000923     C     §3SFL1        OREQ      'S'                                          SOLO SEDE
006000921020     C     SIMFEL        ANDEQ     *ZEROS
006100000923     C     §3SFL1        OREQ      'X'                                          SED+FIL
006200921020     C*
006300921020     C* PASSO NOME DEL FILE
006400921020     C                   MOVEL     *BLANKS       FILE             10
006500980529     C                   MOVEL     §3SARS        FILE
006600980529     C* PASSO NOME DELLA LIBRERIA, SE NON INDICATA IN TABELLE UTILIZZO S.I.
006700980529     C                   MOVEL     *BLANKS       LIBR             10
006800980529     C     §3SLIB        IFEQ      *BLANKS
006900941118     C                   MOVEL     KNSIF         LIBR
007000980529     C                   ELSE
007100980529     C                   MOVEL     §3SLIB        LIBR
007200000703     C                   ENDIF
007300921020     C* PASSO DATA LIMITE PULIZIA
007400921020     C     G02TGI        SUB       §3SCAD        GIOTGI
007500991221     C                   CALL      'XSRGI8'
007600921020     C                   PARM                    WGIDAT
007700921020     C*
007800921020     C                   Z-ADD     0             DATA              6 0
007900921020     C                   Z-ADD     GIOINV        DATA
008000921020     C* PRIMA DEL LANCIO DELLA PULIZIA MI ALLOCO IL FILE SE NON RIESCO
008100921020     C* NON LANCIO
008200050318     C**!!!              Z-ADD     50            LUNG             15 5
008300050318     C**!!!              MOVE      *BLANK        DBASE
008400050318     C**!!!              MOVE      *BLANK        APPI
008500050318     C**!!!              MOVE      *BLANK        CMD
008600050318     C**!!!              MOVEL     '/'           DBASE            11
008700050318     C**!!!              MOVE      §3SARS        DBASE
008800050318     C**!!!CMD1(1)       CAT       LIBR          APPI             22
008900050318     C**!!!APPI          CAT       DBASE:0       CMD              33
009000050318     C**!!!CMD           CAT       CMD2(1)       COMMAN           80
009100050318     C**!!!              CALL      'QCMDEXC'                            60
009200050318     C**!!!              PARM                    COMMAN
009300050318     C**!!!              PARM                    LUNG
009400921020     C*
009500050318     C*!N60              DO
009600921020     C                   MOVEL     PARAM         KPJBU
009700941118     C                   CALL      'TRTC21C'
009800921020     C                   PARM                    KPJBA
009900050318     C**!!!              ENDDO
010000921020     C*
010100000703     C                   ENDIF
010200921020     C*
010300921020     C                   END
010400921020     C                   END
010500921020     C*
010600000000     C                   SETON                                        LR
010700921020     C*------------------------------------------------------------------------*
