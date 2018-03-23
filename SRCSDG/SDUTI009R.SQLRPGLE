000100000000     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200000000     H*PARMS COMMIT(*NONE) DATFMT(*ISO) RDB(*NONE) DYNUSRPRF(*OWNER)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000     H*
000500000000     H*               -------------------------------
000600150930     H* Conversione al nuovo file dei legami SDGLRC00F
000700000000     H*
000800000000     H*
000900000000     F*----------------------------------------------------*
001000000000     D*-------------
001100000000     D* Reperimento nome PGM
001200000000     D STATUS         SDS           333
001300000000     D  DSPGM            *PROC
001400000000     D*------------------
001500000000     D* DS Esterna gestione messaggi
001600000000     D SDGMSGDS      E DS
001700000000     D  STS          E                     EXTFLD(MSGSTS)
001800000000     D                                     DIM(333)
001900000000     D  JBA          E                     EXTFLD(MSGJBA)
002000000000     D                                     DIM(502)
002100000000     D*------------------
002200000000     D SQL             S           2000
002300000000     D*------------------
002400000000     C*----------------------------------------------------*
002500000000     C*                MAIN LINE PROGRAM
002600000000     C*----------------------------------------------------*
002700150930     C*
002800000000     C                   EXSR      ESEGUI
002900000000     C*
003000000000     C                   EXSR      ENDPGM
003100000000     C/EJECT
003200000000     C************************************************************
003300000000     C* Esegui aggiornamenti
003400000000     C************************************************************
003500000000     C     Esegui        BEGSR
003600000000     C*
003700150930     C                   Eval      SQL = 'INSERT INTO SDGLRC00F '
003800160122     C                             + 'SELECT LRCUTEIMM, LRCDTIMM, '
003900150930     C                             + 'LRCUTEULMO, LRCDTULMO, LRCSOCIETA,  '
004000150930     C                             + 'LRCUNITA, LRCSYS, LRCNRASREG,       '
004100160122     C                             + 'LRCLEGAME1, LRCSYSL, LRCNRASL       '
004200150930     C                             + 'FROM NDLRC00F                       '
004300160122     C                             + 'WHERE LRCANN <> ''1''               '
004400160229     C                             + ' and  LRCTIPLEG = ''00001''         '
004500000000     C*
004600000000     C/EXEC SQL
004700000000     C+ EXECUTE IMMEDIATE :SQL
004800000000     C/END-EXEC
004900000000     C* SI È VERIFICATO UN ERRORE
005000000000     C     SqlCod        IFLT      0
005100000000     C                   EXSR      MSGPGM
005200000000     C                   ENDIF
005300000000     C*
005400000000     C                   ENDSR
005500000000     C/EJECT
005600000000     C************************************************************
005700000000     C* Messaggio errore
005800000000     C************************************************************
005900000000     C     MsgPgm        BEGSR
006000000000     C*
006100000000     C                   Reset                   SDGMSGDS
006200000000     C*
006300000000     C                   Movel     'N'           MsgRsp
006400000000     C                   Movel     *Blanks       MsgTpR
006500000000     C                   Z-Add     0             MsgMsg
006600000000     C                   Movel     'S'           MsgEmV
006700000000     C                   Z-Add     10            MsgRgp
006800000000     C                   Movel     'N'           MsgLck
006900000000     C                   Movea     Status        Sts
007000000000     C                   Movel     'N'           MsgCnl
007100000000     C                   Movel     'N'           MsgVid
007200000000     C                   Movel     'N'           MsgOpe
007300000000     C                   Movel     'N'           MsgStp
007400000000     C*
007500000000     C                   Call      'SDGMSGR'
007600000000     C                   Parm                    SDGMSGDS
007700000000     C                   Parm                    XMSGTxt         135
007800000000     C                   Parm                    XMSGInz           1
007900000000     c* codici dei 3 messaggi possibili da emettere a video
008000000000     C* "Si sono verificati errori che hanno impedito l'esecuzione"
008100000000     C                   PARM      'COS0377'     MSGCd1            7
008200000000     C                   PARM                    MSGCd2            7
008300000000     C                   PARM                    MSGCd3            7
008400000000     c* codice del messaggio da mandare alla coda dell'operatore
008500000000     C                   PARM                    MSGSn1            7
008600000000     c* codici(lunghi 7  ciascuno) dei 9 bottoni
008700000000     C                   PARM                    MSGCdTxt         63
008800000000     c* codici(lunghi 7  ciascuno) dei 2 titoli (titolo e sottotitolo)
008900000000     c* per stampa e video
009000000000     C                   PARM                    MSGCdTit         14
009100000000     c* flag per chiusura prtf
009200000000     C                   PARM                    MSGClPrtf         1
009300000000     C* variabili per messaggi
009400000000     C                   PARM                    VarMsg1         100
009500000000     C                   PARM                    VarMsg2         100
009600000000     C                   PARM                    VarMsg3         100
009700000000     C*
009800000000     C                   ENDSR
009900000000     C/EJECT
010000000000     C************************************************************
010100000000     C* OPERAZIONI INIZIALI
010200000000     C************************************************************
010300000000     C     *INZSR        BEGSR
010400000000     C*
010500000000     C                   ENDSR
010600000000     C/EJECT
010700000000     C************************************************************
010800000000     C* Fine programma
010900000000     C************************************************************
011000000000     C     ENDPGM        BEGSR
011100000000     C*
011200000000     C                   SETON                                        LR
011300000000     C                   Return
011400000000     C*
011500000000     C                   ENDSR
011600000000     C/EJECT
