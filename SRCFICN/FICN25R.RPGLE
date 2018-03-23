000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400030203      *  Descrizione   :  Stampa tariffe Cooperative
000500000000      *                   Programma di lancio stampa (filtro)
000600000000      *****************************************************************
000700000000     FAZORG01L  IF   E           K DISK
000800020402     Ftntbe01L  IF   E           K DISK
000900030203     Fficn25D   CF   E             WORKSTN
001000030205      *
001100000000     D KPJBA         E DS
001200030203     D Tibs36ds      E DS
001300000000     D CNCR80        E DS
001400000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001500131007     D tibs42ds      E DS
001600131007     D uteautds      E DS
001700940926     D*
001800020307     D dsparam         DS
001900020307     D  lancio                        1
002000020307     D  padr1                         7s 0
002100020307     D  padr2                         7s 0
002200020307     D  datae                         8s 0
002300030205     D  tipserv                       1
002400030227     D  autconv                       1
002500940926     D*
002600030203     D  utesed         S              1                                         utente di sede
002700940926     D*
002800000000     D WLBDAT          DS
002900940926     D  G02DAT                 1      8  0
003000940926     D  G02INV                 9     16  0
003100940926     D  G02ERR                17     17
003200940926     D  G02TGI                18     22  0
003300000000      *
003400000000     C     *ENTRY        PLIST
003500000000     C                   PARM                    KPJBA
003600030203      *
003700030203      *  In base all'utente prende il P.O.
003800030203     C                   CLEAR                   Tibs36Ds
003900030203     c                   EVAL      I36ute = Knmus
004000030203     c                   EVAL      I36Tla = 'L'
004100030203     C                   CALL      'TIBS36R'
004200030203     C                   PARM                    Tibs36Ds
004300030203      * se utente di sede
004400030203     c                   eval      utesed ='N'
004500030203     c                   if        o36pou = 046
004600030203     c                   seton                                        77
004700030203     c                   eval      utesed ='S'
004800030203     c                   end
004900000000      *--------------------------------------------------------------*
005000000000      *             M A I N      L I N E
005100000000      *--------------------------------------------------------------*
005200000000     C                   Z-ADD     1             CODUT
005300000000     C                   CALL      'X§PARUT'
005400000000     C                   PARM                    UT§DSE
005500000000     C                   MOVEL     RAGUT         RSUT             20
005600000000     C                   MOVEL     RAGUT         RSUT2            20
005700000000     C                   MOVEL     REC80         CNCR80
005800030205      *
005900030205     C* VEDO SE SONO in Filiale o in Sede
006000030205     C                   EVAL      VidFil = O36POU
006100030205     C                   MOVEL     o36Pou        VIDpog            3 0
006200000000      *
006300000000      * Inizializzo i campi a video
006400030205     c                   if        utesed ='S'
006500030205     C                   Z-ADD     *ZERO         VIDFIL                         COD.FILIALE
006600030205     c                   end
006700000000     C                   Z-ADD     *ZERO         VIDDAL                         DATA INIZIALE
006800020307     C                   move      'P'           VIDlan                         DATA FINALE
006900030205     C                   Z-ADD     0             CDPA1V                         1' COD.coop
007000030205     C                   z-add     0             CDPA2V                         2' COD.coop
007100030205     C                   move      'X'           VIDtsr
007200000000      *
007300000000      * Emetto il formato video
007400000000     C     EMETTO        TAG
007500020308     C                   SETOFF                                       90
007600030203     C                   EXFMT     cn25D01
007700000000      *
007800000000     C   KC              GOTO      FINE                                         F3=USCITA
007900000000      *
008000000000      * Eseguo i  controlli sui campi immessi a video
008100000000     C                   EXSR      CONTR
008200000000      *
008300020308      * Se non vi sono errori ma non e' premuto F6 emetto video
008400020308     C  N90
008500020308     CanNkf              SETON                                          90      F6=CONFERMA
008600000000      *
008700000000      * Per errore generico riemetto il formato video
008800000000     C   90              GOTO      EMETTO
008900000000      *
009000020307      * Richiamo programma di stampa
009100020308     c                   if        *inkf
009200020307     c                   move      vidlan        lancio
009300030205     c                   move      vidtsr        TipServ
009400020307     c                   z-add     cdpa1v        padr1
009500020312     c                   movel     vidfil        padr1
009600020307     c                   z-add     cdpa2v        padr2
009700020312     c                   movel     vidfil        padr2
009800020312     c                   z-add     dalrib        datae
009900090903     c                   movel     dsparam       kpjbu
010000090921      * se ristampa non faccio i controlli e permetto di stampare cose già confermate
010100091118     c
010200091118     c                   if        lancio <> 'R' and tipserv = 'X'
010300090903     C                   CALL      'FICN25R1'
010400090903     C                   PARM                    KPJBA
010500090903     c                   movel     kpjbu         dsparam
010600090921     c                   endif
010700090921     c                   select
010800090921     c                   when      autconv = 'E'
010900090903     c                   seton                                        48
011000090903     c                   goto      emetto
011100090921     c                   when      autconv = 'F'
011200090921     c                   seton                                        49
011300090921     c                   goto      emetto
011400090921     c                   other
011500020307     c                   movel     dsparam       kpjbu
011600030203     C                   CALL      'FICN26R'
011700000000     C                   PARM                    KPJBA
011800090921     c                   endsl
011900020308     c                   endif
012000000000      *
012100000000     C     FINE          TAG
012200000000     C                   SETON                                        LR
012300000000      *--------------------------------------------------------------*
012400000000      * Routine per i controlli sui campi del formato video          *
012500000000      *--------------------------------------------------------------*
012600000000     C     CONTR         BEGSR
012700000000      *
012800090921     c                   movea     '000000000'   *in(41)
012900000000      * Verifico il codice filiale
013000000000     C                   MOVE      *BLANKS       DESFIL
013100030205     C     VIDFIL        IFeq      0
013200030205     C                   SETON                                        9041
013300030205     C                   GOTO      ENDCTL
013400030205     c                   else
013500000000     C     VIDFIL        CHAIN     AZORG01L                           41
013600000000     C  N41              MOVEL     ORGDES        DESFIL
013700000000     C   41              SETON                                        90
013800000000     C   41              GOTO      ENDCTL
013900000000      *
014000030205      * IN SEDE QUALSIASI FILIALE, IN FIL SOLO La mia
014100160908     c*m                 if        utesed ='N' and
014200160908     c*m                           o36Pou <> Vidfil
014300160908     C*m                 SETON                                        42  90
014400160908     C*m                 GOTO      ENDCTL
014500160908     c*m                 End
014600160908      *  Controllo Abilitazione alle tariffe
014700160908     c                   clear                   tibs42ds
014800160908     c                   movel     vidfil        i42pge
014900160908     c                   call      'TIBS42R'
015000160908     c                   parm                    TIBS42ds
015100160908     c                   movel     o42uni        uteautds
015200160908      * se abilitato con *in47 abilito la possibilità di lancio
015300160908     c                   if        §autsdtC <> 'S'
015400160908     c                   seton                                        4290
015500160908     C                   GOTO      ENDCTL
015600160908     c                   end
015700030205      *
015800000000     C                   ENDIF
015900000000      *
016000000000      * Controlli di congruenza
016100020312      * Se immesso il cd.padroncino
016200020307     C     CDPA1V        IFeq      *zero
016300020307     C     CDPA2V        ANDeq     *ZERO
016400020307     C                   SETON                                        9045
016500000000     C   90              GOTO      ENDCTL
016600000000     C                   ENDIF
016700020312     C     CDPA1V        IFne      *zero
016800020312     C     CDPA2V        ANDeq     *ZERO
016900020312     C                   z-add     cdpa1v        cdpa2v
017000020312     C                   ENDIF
017100020308     C     CDPA1V        IFne      *zero
017200020308     C     CDPA2V        ANDne     *ZERO
017300020308     C     CDPA1V        andgt     cdpa2v
017400020308     C                   SETON                                        9046
017500020308     C   90              GOTO      ENDCTL
017600020308     C                   ENDIF
017700000000      *
017800000000      * SE IMMESSE SCELTE IMMETTERE ANCHE DATA E VICEVERSA
017900020307     C     VIDDAL        ifeq      0
018000020308     C                   SETON                                        4390
018100000000     C                   GOTO      ENDCTL
018200000000     C                   END
018300030205      *
018400000000      * Controllo data iniziale
018500000000     C     VIDDAL        IFGT      0
018600940926     C                   Z-ADD     VIDDAL        G02DAT
018700000000     C                   MOVEL     *BLANK        G02ERR
018800940926     C                   CALL      'XSRDA8'
018900000000     C                   PARM                    WLBDAT
019000000000     C     G02ERR        IFEQ      '1'
019100020308     C                   SETON                                        44  90
019200000000     C                   GOTO      ENDCTL
019300000000     C                   ENDIF
019400020307     C                   Z-ADD     G02INV        DALRIB            8 0
019500020308     C                   Z-ADD     G02dat        viddal
019600020307     c                   endif
019700020402     c                   setoff                                       47
019800030203      *
019900030203      *  Controllo Abilitazione alle tariffe
020000131007     c                   clear                   tibs42ds
020100131007     c                   movel     vidfil        i42pge
020200131007     c                   call      'TIBS42R'
020300131007     c                   parm                    TIBS42ds
020400131007     c                   movel     o42uni        uteautds
020500030227      * se abilitato con *in47 abilito la possibilità di lancio
020600030227     c                   if        vidlan ='C' and §autsdtC <> 'S'
020700020402     c                   seton                                        4790
020800030227     c                   end
020900030520      *  Per non emettere la dicitura Ristampa
021000030520     c                   if        §autCrSc = 'S'
021100030227     c                   move      'S'           autconv
021200030227     c                   end
021300000000      *
021400000000     C     ENDCTL        ENDSR
