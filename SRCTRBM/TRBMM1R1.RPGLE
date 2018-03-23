000100030401     H*--------------------------------------------------------------------------------------------*
000200030401     H* Inserisce il record nel file di LOG del Mailing Automatico a Clienti
000300030401     H*--------------------------------------------------------------------------------------------*
000400030401     H DECEDIT('0,') DATEDIT(*DMY.)
000500030401     H*--------------------------------------------------------------------------------------------*
000600031211     FTIMAL00F  O  A E             DISK
000700030402     D*------------------
000800030402     D* DS X REPERIMENTO NUMERATORE
000900030402     D*------------------
001000030402     D TRUL33DS      E DS                  INZ
001100030402     D*------------------
001200030402     D* ARCHITETTURA
001300030402     D*------------------
001400030402     D KPJBA         E DS
001500031211     D*------------------
001600031211     D* DS PER GESTIONE DATI DI SERVIZIO SULLA STAMPA
001700031211     D*------------------
001800081101     D TRBMM1DS      E DS
001900031211     D*------------------
002000031211     D* DS CONTROLLO DATA (8)
002100031211     D*------------------
002200031211     D WLBDA8          DS                  INZ
002300031211     D  G08DAT                 1      8  0
002400031211     D  G08INV                 9     16  0
002500031211     D  G08ERR                17     17
002600031211     D  G08TGI                18     22  0
002700031211     D*------------------
002800031211     D* VARIABILI DI WRK
002900031211     D*------------------
003000031211     D  DATAORA        S               Z
003100031211     D  FILNAM         S                   like(MALFNAM)
003200031211     D  JOBNAM         S                   like(MALJNAM)
003300031211     D  USRNAM         S                   like(MALUNAM)
003400031211     D  JOBNUM         S                   like(MALJNUM)
003500031211     D  NUMFIL         S                   like(MALFNUM)
003600080526     D  JOBSYSN        S                   like(MALSYSN)
003700031211     D  USRDTA         S                   like(MALUDTA)
003800031211     D  TOTPAG         S                   like(MALTPAG)
003900040615     D  PRTFLEN        S                   like(MALPRTFL)
004000060705     D  PRTTYP         S                   like(MALPRTTY)
004100040929     D  SPLCPI         S             10  0
004200031211     D  MESSAGGIO      S             80
004300031211     D  ESITO          S              1
004400030401     C*--------------------------------------------------------------------------------------------*
004500030401     C*MAIN
004600030401     C*--------------------------------------------------------------------------------------------*
004700031210     C*
004800030401     C* Ricevimento parametri
004900030401     C     *ENTRY        PLIST
005000031211     C                   PARM                    FILNAM
005100031211     C                   PARM                    JOBNAM
005200031211     C                   PARM                    USRNAM
005300031211     C                   PARM                    JOBNUM
005400031211     C                   PARM                    NUMFIL
005500080526     C                   PARM                    JOBSYSN
005600031211     C                   PARM                    USRDTA
005700031211     C                   PARM                    TOTPAG
005800040615     C                   PARM                    PRTFLEN
005900040722     C                   PARM                    SPLCPI
006000081101     C                   PARM                    TRBMM1DS
006100060705     C                   PARM                    PRTTYP
006200031211     C                   PARM                    MESSAGGIO
006300031211     C                   PARM                    ESITO
006400030401     C*
006500030401     C* Reperisco data e ora correnti
006600030401     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
006700030401     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
006800030401     C                   MOVEL     WN14          ORACOR            6            *ORA  (8) IN H/M/SS
006900030401     C                   Z-ADD     WN8           G08DAT
007000030401     C                   Z-ADD     *ZEROS        G08INV
007100030401     C                   MOVEL     '0'           G08ERR
007200030401     C                   CALL      'XSRDA8'
007300030401     C                   PARM                    WLBDA8
007400030401     C                   MOVEL     G08INV        DATCOR            8            *DATA CORRENTE AA/M/
007500030402     C                   TIME                    DATAORA
007600030401     C*
007700041001     C* Se no errore => se previsto stacco un progressivo mail
007800030402     C                   CLEAR                   TRUL33DS
007900030402     C                   IF        ESITO <> '2'
008000041005     C                   IF        低M1PROG = *blanks
008100030402     C                   EXSR      REPNUM
008200041001     C                   ELSE
008300041005     C                   MOVE(P)   低M1PROG      O33NRF
008400041001     C                   ENDIF
008500030402     C                   ENDIF
008600030401     C*
008700030402     C* Inserisco il record nel file di log Mailing Automatico
008800030401     C                   CLEAR                   TIMAL000
008900030402     C                   MOVE(P)   O33NRF        MALPROG
009000030402     C                   EVAL      MALDAOR = %char(DATAORA)
009100030401     C                   EVAL      MALFNAM = FILNAM
009200030401     C                   EVAL      MALJNAM = JOBNAM
009300030401     C                   EVAL      MALUNAM = USRNAM
009400030401     C                   EVAL      MALJNUM = JOBNUM
009500030401     C                   EVAL      MALFNUM = NUMFIL
009600080526     C                   EVAL      MALSYSN = JOBSYSN
009700081101     C                   EVAL      MALDUEX = TRBMM1DS
009800030604     C                   EVAL      MALUDTA = USRDTA
009900031211     C                   Z-ADD     TOTPAG        MALTPAG
010000040615     C                   Z-ADD     PRTFLEN       MALPRTFL
010100040929     C                   Z-ADD     SPLCPI        MALSPCPI
010200060705     C                   EVAL      MALPRTTY= PRTTYP
010300030402     C                   EVAL      MALMSG  = MESSAGGIO
010400030604     C                   EVAL      MALSTS  = ESITO
010500031210     C                   IF        (ESITO <> '2' AND
010600031210     C                              低M1STS = 'H') OR
010700031210     C                             %trim(MALDUEX) = '*NONE'
010800031210     C                   EVAL      MALSTS  = 'H'
010900030604     C                   ENDIF
011000040914     C                   EVAL      MALIDP = 低M1IDP
011100041008     C*
011200041008     C* Se il numero spool corrente / numero spool totali nn sono valorizzati li forzo
011300041008     C* entrambi a '01'
011400041008     C                   IF        低M1ATT = *blanks
011500041008     C                   EVAL      低M1ATT = '01'
011600041008     C                   ENDIF
011700041008     C                   IF        低M1TOTATT = *blanks
011800041008     C                   EVAL      低M1TOTATT = '01'
011900041008     C                   ENDIF
012000041008     C*
012100041005     C                   EVAL      MALATT = 低M1ATT
012200041005     C                   EVAL      MALTOTATT = 低M1TOTATT
012300030402     C                   WRITE     TIMAL000
012400001006     C*
012500030401     C                   RETURN
012600030402     C*--------------------------------------------------------------------------------------------*
012700030402     C* REPNUM - REPERISCE NUMERATORE
012800030402     C*--------------------------------------------------------------------------------------------*
012900030402     C     REPNUM        BEGSR
013000030402     C*
013100030402     C* INIALIAZIZZO FLAG ERRORE
013200030402     C                   EVAL      I33TLA = 'L'
013300030402     C                   EVAL      I33OPE = *zeros
013400030402     C                   EVAL      I33CNU = 024
013500030402     C                   EVAL      I33NUM = 1
013600030402     C                   MOVEL     TRUL33DS      KPJBU
013700041025     C                   CALL      'TRUL33C'
013800030402     C                   PARM                    KPJBA
013900030402     C                   MOVEL     KPJBU         TRUL33DS
014000030402     C                   IF        O33ERR <> *zeros
014100030402     C                   MOVEL     '2'           ESITO
014200081101     C                   EVAL      MESSAGGIO = 'TRBMM1R1 - Errore in ' +
014300041025     C                                         'reperimento contatore (TRUL33C)'
014400030402     C                   ENDIF
014500030402     C*
014600030402     C                   ENDSR
