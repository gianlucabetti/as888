000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400020402      *  Descrizione   :  conferma ricezione stampa tariffe padroncini
000500000000      *                   Programma di lancio stampa (filtro)
000600000000      *****************************************************************
000700000000     FAZORG01L  IF   E           K DISK
000800020402     Ftntbe01L  IF   E           K DISK
000900021203     Ffiapd01l  if   e           k disk
001000020402     Fficn16D   CF   E             WORKSTN
001100070308     F                                     maxdev(*file)
001200000000      *
001300960913     D* SCHIERE PER CARICAMENTO L1
001400940926     D L1              S              3  0 DIM(30)                              FIL GESTITE
001500940926     D*
001600000000     D KPJBA         E DS
001700020701     D* REM                    1      3
001800020701     D* REMFIL                 4      6
001900000000     D CNCR80        E DS
002000000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002100020402     D ficn70ds      E DS
002200020402     D daut          E DS
002300940926     D*
002400030203     D tipo            s              1
002500020307     D dsparam         DS
002600020307     D  padr1                         7s 0
002700020307     D  datae                         8s 0
002800030416     D  serv1                         1
002900940926     D*
003000940926     D* DS PER TRUL06R - CARICAMENTO £1
003100040309     D*DSUL06        E DS                  EXTNAME(TRUL06DS)
003200040309     D* LIN                    1     90  0
003300940926     D                                     DIM(30)                              SKI COMODO
003400940926     D*
003500000000     D WLBDAT          DS
003600940926     D  G02DAT                 1      8  0
003700940926     D  G02INV                 9     16  0
003800940926     D  G02ERR                17     17
003900940926     D  G02TGI                18     22  0
004000000000      *
004100000000     C     *ENTRY        PLIST
004200000000     C                   PARM                    KPJBA
004300021203     c     kapd          klist
004400030203     c                   kfld                    tipo
004500021203     c                   kfld                    kpdr
004600030203     c                   movel     kpjbu         tipo
004700030416     c     tipo          comp      'C'                                    49
004800000000      *
004900000000      *--------------------------------------------------------------*
005000000000      *             M A I N      L I N E
005100000000      *--------------------------------------------------------------*
005200000000     C                   Z-ADD     1             CODUT
005300000000     C                   CALL      'X§PARUT'
005400000000     C                   PARM                    UT§DSE
005500000000     C                   MOVEL     RAGUT         RSUT             20
005600000000     C                   MOVEL     RAGUT         RSUT2            20
005700000000     C                   MOVEL     REC80         CNCR80
005800000000      *
005900000000     C                   EXSR      DEFCAM                                       INIZIALIZZ.
006000000000      *
006100000000      * Inizializzo i campi a video
006200000000     C                   Z-ADD     *ZERO         VIDFIL                         COD.FILIALE
006300000000     C                   Z-ADD     *ZERO         VIDDAL                         DATA INIZIALE
006400020307     C                   Z-ADD     0             CDPA1V                         1' COD.PADRONC.
006500000000      *
006600000000      * Emetto il formato video
006700000000     C     EMETTO        TAG
006800020308     C                   SETOFF                                       90
006900070308     C                   write     cn16D01
007000070308     c                   read(e)   ficn16d
007100070308     c                   if        %error
007200070308     c                   eval      *inkc = '1'
007300070308     c                   endif
007400000000      *
007500000000     C   KC              GOTO      FINE                                         F3=USCITA
007600000000      *
007700000000      * Eseguo i  controlli sui campi immessi a video
007800000000     C                   EXSR      CONTR
007900000000      *
008000020308      * Se non vi sono errori ma non e' premuto F6 emetto video
008100020308     C  N90
008200020308     CanNkf              SETON                                          90      F6=CONFERMA
008300000000      *
008400000000      * Per errore generico riemetto il formato video
008500000000     C   90              GOTO      EMETTO
008600000000      *
008700020307      * Richiamo programma di stampa
008800020308     c                   if        *inkf
008900020307     c                   z-add     cdpa1v        padr1
009000020312     c                   movel     vidfil        padr1
009100020312     c                   z-add     dalrib        datae
009200030416     c                   move      vidtsr        serv1
009300020307     c                   movel     dsparam       kpjbu
009400030203     c                   if        tipo = 'A'
009500020402     C                   CALL      'FICN17R'
009600000000     C                   PARM                    KPJBA
009700030203     c                   else
009800030203     C                   CALL      'FICN17R1'
009900030203     C                   PARM                    KPJBA
010000030203     c                   endif
010100020308     c                   endif
010200000000      *
010300000000     C     FINE          TAG
010400000000     C                   SETON                                        LR
010500000000      *---------------------------------------------------------------*
010600000000      * Definizioni/dichiarazioni iniziali                            *
010700000000      *---------------------------------------------------------------*
010800000000     C     DEFCAM        BEGSR
010900940926     C***
011000940926     C* CARICO TABELLA FILIALI GESTITE £1
011100940926     C***
011200040309     C*                  CLEAR                   DSUL06
011300040309     C*                  MOVE      '£1'          D06COD
011400040309     C*    SIMFEL        IFGT      0
011500040309     C*                  MOVEL     SIMFEL        D06KEY
011600020402     C                   MOVEL     simfel        VIDpog            3 0
011700040309     C*                  ENDIF
011800040309     C*                  MOVEL     DSUL06        KPJBU
011900940926     C*
012000040309     C*                  CALL      'TRUL06R'
012100040309     C*                  PARM                    KPJBA
012200040309     C*                  MOVEL     KPJBU         DSUL06
012300040309     C*                  MOVEA     LIN           L1
012400020402     C* VEDO SE SONO SIMFEL O REMOTO
012500020701     C                   EVAL      VidFil = SimPOU
012600020701     C                   EVAL      VidPOG = SimPOU
012700000000     C                   ENDSR
012800000000      *--------------------------------------------------------------*
012900000000      * Routine per i controlli sui campi del formato video          *
013000000000      *--------------------------------------------------------------*
013100000000     C     CONTR         BEGSR
013200000000      *
013300020308     c                   movea     '000000'      *in(41)
013400030416     c                   setoff                                       48
013500000000      * Verifico il codice filiale
013600000000     C                   MOVE      *BLANKS       DESFIL
013700000000     C     VIDFIL        IFGT      0
013800000000     C     VIDFIL        CHAIN     AZORG01L                           41
013900000000     C  N41              MOVEL     ORGDES        DESFIL
014000000000     C   41              SETON                                        90
014100000000     C   41              GOTO      ENDCTL
014200000000      *
014300000000      * IN SEDE QUALSIASI FILIALE, IN FIL SOLO LE MIE
014400040305     C*    SIMFEL        IFNE      0
014500040305     C*    VIDFIL        LOOKUP    L1                                     99
014600040305     C* N99              SETON                                        42  90
014700040305     C* N99              GOTO      ENDCTL
014800040305     C*                  ENDIF
014900020308     c                   else
015000020308     C                   SETON                                        9041
015100020308     C                   GOTO      ENDCTL
015200000000     C                   ENDIF
015300000000      *
015400000000      * Controlli di congruenza
015500020312      * Se immesso il cd.padroncino
015600020307     C     CDPA1V        IFeq      *zero
015700020307     C                   SETON                                        9045
015800000000     C   90              GOTO      ENDCTL
015900000000     C                   ENDIF
016000000000      *
016100000000      * SE IMMESSE SCELTE IMMETTERE ANCHE DATA E VICEVERSA
016200020307     C     VIDDAL        ifeq      0
016300020308     C                   SETON                                        4390
016400000000     C                   GOTO      ENDCTL
016500000000     C                   END
016600000000      * Controllo data iniziale
016700000000     C     VIDDAL        IFGT      0
016800940926     C                   Z-ADD     VIDDAL        G02DAT
016900000000     C                   MOVEL     *BLANK        G02ERR
017000940926     C                   CALL      'XSRDA8'
017100000000     C                   PARM                    WLBDAT
017200000000     C     G02ERR        IFEQ      '1'
017300020308     C                   SETON                                        44  90
017400000000     C                   GOTO      ENDCTL
017500000000     C                   ENDIF
017600020307     C                   Z-ADD     G02INV        DALRIB            8 0
017700020308     C                   Z-ADD     G02dat        viddal
017800020307     c                   endif
017900030416      * SE coop deve essere gestito il tipo servizio
018000030416     C                   if        tipo = 'C' and vidtsr = *blank
018100030416     C                   SETON                                        4890
018200030416     C                   GOTO      ENDCTL
018300030416     C                   END
018400020402     c                   movel     vidfil        kpdr              7 0
018500020402     c                   move      cdpa1v        kpdr
018600021203     c     kapd          chain     fiapd01l
018700020402     c                   movel     apdrsc        despad
018800020402     c                   setoff                                       47
018900020402     c                   movel     vidpog        i70pog
019000020402     c                   movel     vidfil        i70pge
019100020402     c                   movel     knmus         i70prf
019200020402     c                   call      'FICN70R'
019300020402     c                   parm                    ficn70ds
019400020402     c                   movel     o70uni        daut
019500020402      * se abilitato con *in47 abilito la possibilità di lancio
019600030203     c                   if        tipo = 'A'
019700020402     c                   if        §autcrs <> 'S'
019800020402     c                   seton                                        4790
019900020402     c                   endif
020000030203     c                   else
020100030203     c                   if        §autcrsc <> 'S'
020200030203     c                   seton                                        4790
020300030203     c                   endif
020400030203     c                   endif
020500000000      *
020600000000     C     ENDCTL        ENDSR
