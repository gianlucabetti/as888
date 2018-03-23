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
001800031211     D TRTCM1DS      E DS
001900031211     D*------------------
002000031211     D* VARIABILI DI WRK
002100031211     D*------------------
002200031211     D  DATAORA        S               Z
002300031211     D  FILNAM         S                   like(MALFNAM)
002400031211     D  JOBNAM         S                   like(MALJNAM)
002500031211     D  USRNAM         S                   like(MALUNAM)
002600031211     D  JOBNUM         S                   like(MALJNUM)
002700031211     D  NUMFIL         S                   like(MALFNUM)
002800080526     D  JOBSYSN        S                   like(MALSYSN)
002900031211     D  USRDTA         S                   like(MALUDTA)
003000031211     D  TOTPAG         S                   like(MALTPAG)
003100040615     D  PRTFLEN        S                   like(MALPRTFL)
003200060705     D  PRTTYP         S                   like(MALPRTTY)
003300040929     D  SPLCPI         S             10  0
003400031211     D  MESSAGGIO      S             80
003500031211     D  ESITO          S              1
003600030401     C*--------------------------------------------------------------------------------------------*
003700030401     C*MAIN
003800030401     C*--------------------------------------------------------------------------------------------*
003900031210     C*
004000030401     C* Ricevimento parametri
004100030401     C     *ENTRY        PLIST
004200031211     C                   PARM                    FILNAM
004300031211     C                   PARM                    JOBNAM
004400031211     C                   PARM                    USRNAM
004500031211     C                   PARM                    JOBNUM
004600031211     C                   PARM                    NUMFIL
004700080526     C                   PARM                    JOBSYSN
004800031211     C                   PARM                    USRDTA
004900031211     C                   PARM                    TOTPAG
005000040615     C                   PARM                    PRTFLEN
005100040722     C                   PARM                    SPLCPI
005200031211     C                   PARM                    TRTCM1DS
005300060705     C                   PARM                    PRTTYP
005400031211     C                   PARM                    MESSAGGIO
005500031211     C                   PARM                    ESITO
005600030401     C*
005700030401     C* Reperisco data e ora correnti
005800030401     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
005900030401     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
006000030401     C                   MOVEL     WN14          ORACOR            6            *ORA  (8) IN H/M/SS
006100100111     C                   Z-ADD     *ZEROS        NUMDATCOR         8 0
006200100111     C                   EVAL      NUMDATCOR = %DEC(%DATE() : *ISO)
006300030402     C                   TIME                    DATAORA
006400030401     C*
006500041001     C* Se no errore => se previsto stacco un progressivo mail
006600030402     C                   CLEAR                   TRUL33DS
006700030402     C                   IF        ESITO <> '2'
006800041005     C                   IF        低M1PROG = *blanks
006900030402     C                   EXSR      REPNUM
007000041001     C                   ELSE
007100041005     C                   MOVE(P)   低M1PROG      O33NRF
007200041001     C                   ENDIF
007300030402     C                   ENDIF
007400030401     C*
007500030402     C* Inserisco il record nel file di log Mailing Automatico
007600030401     C                   CLEAR                   TIMAL000
007700030402     C                   MOVE(P)   O33NRF        MALPROG
007800030402     C                   EVAL      MALDAOR = %char(DATAORA)
007900030401     C                   EVAL      MALFNAM = FILNAM
008000030401     C                   EVAL      MALJNAM = JOBNAM
008100030401     C                   EVAL      MALUNAM = USRNAM
008200030401     C                   EVAL      MALJNUM = JOBNUM
008300030401     C                   EVAL      MALFNUM = NUMFIL
008400080526     C                   EVAL      MALSYSN = JOBSYSN
008500030401     C                   EVAL      MALDUEX = TRTCM1DS
008600030604     C                   EVAL      MALUDTA = USRDTA
008700031211     C                   Z-ADD     TOTPAG        MALTPAG
008800040615     C                   Z-ADD     PRTFLEN       MALPRTFL
008900040929     C                   Z-ADD     SPLCPI        MALSPCPI
009000060705     C                   EVAL      MALPRTTY= PRTTYP
009100030402     C                   EVAL      MALMSG  = MESSAGGIO
009200030604     C                   EVAL      MALSTS  = ESITO
009300031210     C                   IF        (ESITO <> '2' AND
009400031210     C                              低M1STS = 'H') OR
009500031210     C                             %trim(MALDUEX) = '*NONE'
009600031210     C                   EVAL      MALSTS  = 'H'
009700030604     C                   ENDIF
009800040914     C                   EVAL      MALIDP = 低M1IDP
009900041008     C*
010000041008     C* Se il numero spool corrente / numero spool totali nn sono valorizzati li forzo
010100041008     C* entrambi a '01'
010200041008     C                   IF        低M1ATT = *blanks
010300041008     C                   EVAL      低M1ATT = '01'
010400041008     C                   ENDIF
010500041008     C                   IF        低M1TOTATT = *blanks
010600041008     C                   EVAL      低M1TOTATT = '01'
010700041008     C                   ENDIF
010800041008     C*
010900041005     C                   EVAL      MALATT = 低M1ATT
011000041005     C                   EVAL      MALTOTATT = 低M1TOTATT
011100030402     C                   WRITE     TIMAL000
011200001006     C*
011300030401     C                   RETURN
011400030402     C*--------------------------------------------------------------------------------------------*
011500030402     C* REPNUM - REPERISCE NUMERATORE
011600030402     C*--------------------------------------------------------------------------------------------*
011700030402     C     REPNUM        BEGSR
011800030402     C*
011900030402     C* INIALIAZIZZO FLAG ERRORE
012000030402     C                   EVAL      I33TLA = 'L'
012100030402     C                   EVAL      I33OPE = *zeros
012200030402     C                   EVAL      I33CNU = 024
012300030402     C                   EVAL      I33NUM = 1
012400030402     C                   MOVEL     TRUL33DS      KPJBU
012500041025     C                   CALL      'TRUL33C'
012600030402     C                   PARM                    KPJBA
012700030402     C                   MOVEL     KPJBU         TRUL33DS
012800030402     C                   IF        O33ERR <> *zeros
012900030402     C                   MOVEL     '2'           ESITO
013000041001     C                   EVAL      MESSAGGIO = 'TRTCM1R1 - Errore in ' +
013100041025     C                                         'reperimento contatore (TRUL33C)'
013200030402     C                   ENDIF
013300030402     C*
013400030402     C                   ENDSR
