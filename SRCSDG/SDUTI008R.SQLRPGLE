000100150212     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200150212     H*PARMS COMMIT(*NONE) DATFMT(*ISO) RDB(*NONE) DYNUSRPRF(*OWNER)
000300150212     H DECEDIT('0,') DATEDIT(*DMY.)
000400150212     H*
000500150212     H*               -------------------------------
000600150212     H* Conversione al nuovo file ND77000F
000700150212     H*
000800150212     H*
000900150212     F*----------------------------------------------------*
001000150212     FQADSPFFD  IF   E             DISK    USROPN
001001150212     D*-------------
001100150212     D Ncampi          S                   DIM(100) Like(WHFLDI) inz            X QCMDEXC
001200150212     D CM1             S             80    DIM(3) CTDATA PERRCD(1)              X QCMDEXC
001300150212     D*-------------
001400150212     D* Reperimento nome PGM
001500150212     D STATUS         SDS           333
001600150212     D  DSPGM            *PROC
001700150212     D*------------------
001800150212     D* DS Esterna gestione messaggi
001900150212     D SDGMSGDS      E DS
002000150212     D  STS          E                     EXTFLD(MSGSTS)
002100150212     D                                     DIM(333)
002200150212     D  JBA          E                     EXTFLD(MSGJBA)
002300150212     D                                     DIM(502)
002400150212     D*------------------
002500150212     D $IND            S              3  0
002600150212     D*------------------
002700150212     D SQL             S           2000
002800150212     D*------------------
002900150212     C*----------------------------------------------------*
003000150212     C*                MAIN LINE PROGRAM
003100150212     C*----------------------------------------------------*
003200150212     C                   EXSR      REPCAMPI
003300150212     C*
003400150212     C                   EXSR      ESEGUI
003500150212     C*
003600150212     C                   EXSR      ENDPGM
003700150212     C/EJECT
003701150212     C************************************************************
003702150212     C* Reperimento campi del file
003703150212     C************************************************************
003704150212     C     Repcampi      BEGSR
003705150212     C*
003706150212     C* Delete
003707150212     C                   CALL      'QCMDEXC'                            21
003708150212     C                   PARM                    CM1(1)
003709150212     C                   PARM      80            LEN              15 5
003710150212     C*
003711150212     C* OVRDBF
003712150212     C                   CALL      'QCMDEXC'                            21
003713150212     C                   PARM                    CM1(2)
003714150212     C                   PARM      80            LEN              15 5
003715150212     C*
003716150212     C* DSPFFD
003717150212     C                   CALL      'QCMDEXC'                            21
003718150212     C                   PARM                    CM1(3)
003719150212     C                   PARM      80            LEN              15 5
003720150212     C*
003721150212     C     *IN21         IFEQ      *OFF
003722150212     C*
003723150212     C                   OPEN      QADSPFFD
003724150212     C     1             SETLL     QADSPFFD                           21
003725150212     C*
003726150212     C     *IN21         DOWEQ     *OFF
003727150212     C*
003728150212     C                   READ      QADSPFFD                               21
003729150212     C                   ADD       1             $IND
003730150212     C                   MOVEL     WHFLDI        NCAMPI($Ind)
003731150212     C                   ENDDO
003732150212     C*
003733150212     C                   CLOSE     QADSPFFD
003734150212     C*
003735150212     C                   ENDIF
003736150212     C*
003738150212     C                   ENDSR
003739150212     C/EJECT
003800150212     C************************************************************
003900150212     C* Esegui aggiornamenti
004000150212     C************************************************************
004100150212     C     Esegui        BEGSR
004200150212     C*
004300150212     C                   Eval      SQL = 'INSERT INTO ND77000F '
004400150212     C                             + 'SELECT N77SOCIETA, N77ACF, N77QUA,  '
004500150212     C                             + 'N77KCC, N77KSC, N77RGT, N77CDF,     '
004600150212     C                             + 'N77COG, N77NOM, N77LNC, N77DNC,     '
004700150212     C                             + 'N77PNC, N77SES, N77CIT, N77VIA,     '
004800150212     C                             + 'N77NUM, N77PRV, N77C77, N77ITC,     '
004900150212     C                             + 'N77IMN, N77SNS, N77IMS, N77PRA,     '
005000150212     C                             + 'N77RAC, N77NET, N77PFG, N77SER,     '
005100150212     C                             + 'N77CSE, N77CDE, N77PRG, N77REG,     '
005200150212     C                             + 'N77TIT, N77ANNO, N77ARG, N77ARS,    '
005300150212     C                             + 'N77IMSAP, N77RACAP, N77PIN, N77DTIN,'
005400150212     C                             + 'N77DTFI, N77COM, N77RCDF,           '
005500150212     C                             + 'N77RCOG, N77RNOM, N77RSES, N77RLNC, '
005600150212     C                             + 'N77RDNC, N77RPNC, N77RCIT,          '
005700150212     C                             + 'N77RVIA, N77RPRV, N77RCSE, N77RIM,  '
005701150212     C                             + 'N77ARI, N77CTBDL, N77CTBLV,         '
005702150212     C                             + 'N77SPERIMB, N77RITRIMB, N77EREDE,   '
005703150212     C                             + 'N77EVENTI, N77IMPPRFA, N77IMPCUCO,  '
008400150212     C*
008401150212     C* 2009 - C2157 ==> R024
008402150212     C                   Select
008500150212     C                   When      %lookup('N77CDFX   ': Ncampi) > 0
008600150212     C                   Eval      SQL=%TRIM(SQL)+      (' 0, N77CDFX, +
008700150212     C                             N77IMSX, N77RACX, N77RIMX, N77NETX, +
008800150212     C                             N77ARGX, N77ARIX, N77ARSX,')
008801150212     C*
008802150212     C* 2009 - R024  ==> R024
008901150212     C                   When      %lookup('N77ASIMP  ': Ncampi) > 0
008902150212     C                   Eval      SQL=%TRIM(SQL)+      ('N77ASIMP, N77ASCDF, +
008903150212     C                             N77ASIMS, N77ASRAC, N77ASRIM, N77ASRSO, +
008904150212     C                             N77ASARA, N77ASARI, N77ASARS, ')
008905150212     C                   Other
009000150212     C                   Eval      SQL=%TRIM(SQL)+      (' 0, '' '', 0, 0, 0, +
009100150212     C                             0, 0, 0, 0, ')
009200150212     C                   endsl
009300150212     C*
009301150212     C*
009302150212     C* 2010 - C2188 ==> R055
009303150212     C                   Select
009400150212     C                   When      %lookup('N77C77X   ': Ncampi) > 0
009500150212     C                   Eval      SQL=%TRIM(SQL)+      ('N77C77X, ')
009501150212     C*
009502150212     C* 2010 - R055  ==> R055
009503150212     C                   When      %lookup('N77CAUAS  ': Ncampi) > 0
009504150212     C                   Eval      SQL=%TRIM(SQL)+      ('N77CAUAS,  ')
009505150212     C*
009600150212     C                   Other
009700150212     C                   Eval      SQL=%TRIM(SQL)+      (''' '', ')
009800150212     C                   endSl
009801150212     C*
009802150212     C*
009803150212     C* 2012 - C2122 ==> R110
009804150212     C                   Select
010000150212     C                   When      %lookup('N77CSN    ': Ncampi) > 0
010100150212     C                   Eval      SQL=%TRIM(SQL)+      ('N77CSN, ')
010101150212     C*
010102150212     C* 2012 - R110  ==> R110
010103150212     C                   When      %lookup('N77CODICE ': Ncampi) > 0
010104150212     C                   Eval      SQL=%TRIM(SQL)+      ('N77CODICE, ')
010201150212     C*
010202150212     C                   Other
010300150212     C                   Eval      SQL=%TRIM(SQL)+      (''' '', ')
010400150212     C                   endSl
010500150212     C*
010502150212     C*
010503150212     C* 2013 - C2236 ==> R148
010504150212     C                   Select
010600150212     C                   When      %lookup('N77ACI    ': Ncampi) > 0
010700150212     C                   Eval      SQL=%TRIM(SQL)+      ('N77ACI, N77ACS, +
010800150212     C                             N77ACG,N77ACIX, N77ACSX, N77ACGX,')
010801150212     C*
010802150212     C* 2013 - R148  ==> R148
010804150212     C                   When      %lookup('N77ACTA   ': Ncampi) > 0
010805150212     C                   Eval      SQL=%TRIM(SQL)+      ('N77ACG, N77ACS, +
010806150212     C                             N77ACTA, N77ASACI, N77ASACS, N77ASACA,')
010900150212     C                   Other
011000150212     C
011100150212     C                   Eval      SQL=%TRIM(SQL)+ (' 0 , 0 , 0 , 0 , 0 , 0 ,')
011200150212     C                   endSl
011201150212     C*
011202150212     C* 2014 -       ==> R165
011400150212     C                   Eval      SQL=%TRIM(SQL) +      ('''  '' ,   +
011500150212     C                              '' ''  FROM ND770SVF')
011600150212     C*
011700150212     C/EXEC SQL
011800150212     C+ EXECUTE IMMEDIATE :SQL
011900150212     C/END-EXEC
012000150212     C* SI È VERIFICATO UN ERRORE
012100150212     C     SqlCod        IFLT      0
012200150212     C                   EXSR      MSGPGM
012300150212     C                   ENDIF
012400150212     C*
012500150212     C                   ENDSR
012600150212     C/EJECT
012700150212     C************************************************************
012800150212     C* Messaggio errore
012900150212     C************************************************************
013000150212     C     MsgPgm        BEGSR
013100150212     C*
013200150212     C                   Reset                   SDGMSGDS
013300150212     C*
013400150212     C                   Movel     'N'           MsgRsp
013500150212     C                   Movel     *Blanks       MsgTpR
013600150212     C                   Z-Add     0             MsgMsg
013700150212     C                   Movel     'S'           MsgEmV
013800150212     C                   Z-Add     10            MsgRgp
013900150212     C                   Movel     'N'           MsgLck
014000150212     C                   Movea     Status        Sts
014100150212     C                   Movel     'N'           MsgCnl
014200150212     C                   Movel     'N'           MsgVid
014300150212     C                   Movel     'N'           MsgOpe
014400150212     C                   Movel     'N'           MsgStp
014500150212     C*
014600150212     C                   Call      'SDGMSGR'
014700150212     C                   Parm                    SDGMSGDS
014800150212     C                   Parm                    XMSGTxt         135
014900150212     C                   Parm                    XMSGInz           1
015000150212     c* codici dei 3 messaggi possibili da emettere a video
015100150212     C* "Si sono verificati errori che hanno impedito l'esecuzione"
015200150212     C                   PARM      'COS0377'     MSGCd1            7
015300150212     C                   PARM                    MSGCd2            7
015400150212     C                   PARM                    MSGCd3            7
015500150212     c* codice del messaggio da mandare alla coda dell'operatore
015600150212     C                   PARM                    MSGSn1            7
015700150212     c* codici(lunghi 7  ciascuno) dei 9 bottoni
015800150212     C                   PARM                    MSGCdTxt         63
015900150212     c* codici(lunghi 7  ciascuno) dei 2 titoli (titolo e sottotitolo)
016000150212     c* per stampa e video
016100150212     C                   PARM                    MSGCdTit         14
016200150212     c* flag per chiusura prtf
016300150212     C                   PARM                    MSGClPrtf         1
016400150212     C* variabili per messaggi
016500150212     C                   PARM                    VarMsg1         100
016600150212     C                   PARM                    VarMsg2         100
016700150212     C                   PARM                    VarMsg3         100
016800150212     C*
016900150212     C                   ENDSR
017000150212     C/EJECT
020600150212     C************************************************************
020700150212     C* OPERAZIONI INIZIALI
020800150212     C************************************************************
020900150212     C     *INZSR        BEGSR
021000150212     C*
021100150212     C                   ENDSR
021200150212     C/EJECT
021300150212     C************************************************************
021400150212     C* Fine programma
021500150212     C************************************************************
021600150212     C     ENDPGM        BEGSR
021700150212     C*
021800150212     C                   SETON                                        LR
021900150212     C                   Return
022000150212     C*
022100150212     C                   ENDSR
022101150212     C/EJECT
022200150212**
022300150212DLTF FILE(QTEMP/QADSPFFD)
022400150212DSPFFD FILE(ND770SVF) OUTPUT(*OUTFILE) OUTFILE(QTEMP/QADSPFFD)
022500150212OVRDBF FILE(QADSPFFD) TOFILE(QTEMP/QADSPFFD)
