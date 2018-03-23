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
010001091118     c
010100091118     c                   if        lancio <> 'R' and tipserv = 'X'
010200090903     C                   CALL      'FICN25R1'
010300090903     C                   PARM                    KPJBA
010400090903     c                   movel     kpjbu         dsparam
010500090921     c                   endif
010600090921     c                   select
010700090921     c                   when      autconv = 'E'
010800090903     c                   seton                                        48
010900090903     c                   goto      emetto
011000090921     c                   when      autconv = 'F'
011100090921     c                   seton                                        49
011200090921     c                   goto      emetto
011300090921     c                   other
011400020307     c                   movel     dsparam       kpjbu
011500030203     C                   CALL      'FICN26R'
011600000000     C                   PARM                    KPJBA
011700090921     c                   endsl
011800020308     c                   endif
011900000000      *
012000000000     C     FINE          TAG
012100000000     C                   SETON                                        LR
012200000000      *--------------------------------------------------------------*
012300000000      * Routine per i controlli sui campi del formato video          *
012400000000      *--------------------------------------------------------------*
012500000000     C     CONTR         BEGSR
012600000000      *
012700090921     c                   movea     '000000000'   *in(41)
012800000000      * Verifico il codice filiale
012900000000     C                   MOVE      *BLANKS       DESFIL
013000030205     C     VIDFIL        IFeq      0
013100030205     C                   SETON                                        9041
013200030205     C                   GOTO      ENDCTL
013300030205     c                   else
013400000000     C     VIDFIL        CHAIN     AZORG01L                           41
013500000000     C  N41              MOVEL     ORGDES        DESFIL
013600000000     C   41              SETON                                        90
013700000000     C   41              GOTO      ENDCTL
013800000000      *
013900030205      * IN SEDE QUALSIASI FILIALE, IN FIL SOLO La mia
014000030205     c                   if        utesed ='N' and
014100030205     c                             o36Pou <> Vidfil
014200030205     C                   SETON                                        42  90
014300030205     C                   GOTO      ENDCTL
014400030205     c                   End
014500030205      *
014600000000     C                   ENDIF
014700000000      *
014800000000      * Controlli di congruenza
014900020312      * Se immesso il cd.padroncino
015000020307     C     CDPA1V        IFeq      *zero
015100020307     C     CDPA2V        ANDeq     *ZERO
015200020307     C                   SETON                                        9045
015300000000     C   90              GOTO      ENDCTL
015400000000     C                   ENDIF
015500020312     C     CDPA1V        IFne      *zero
015600020312     C     CDPA2V        ANDeq     *ZERO
015700020312     C                   z-add     cdpa1v        cdpa2v
015800020312     C                   ENDIF
015900020308     C     CDPA1V        IFne      *zero
016000020308     C     CDPA2V        ANDne     *ZERO
016100020308     C     CDPA1V        andgt     cdpa2v
016200020308     C                   SETON                                        9046
016300020308     C   90              GOTO      ENDCTL
016400020308     C                   ENDIF
016500000000      *
016600000000      * SE IMMESSE SCELTE IMMETTERE ANCHE DATA E VICEVERSA
016700020307     C     VIDDAL        ifeq      0
016800020308     C                   SETON                                        4390
016900000000     C                   GOTO      ENDCTL
017000000000     C                   END
017100030205      *
017200000000      * Controllo data iniziale
017300000000     C     VIDDAL        IFGT      0
017400940926     C                   Z-ADD     VIDDAL        G02DAT
017500000000     C                   MOVEL     *BLANK        G02ERR
017600940926     C                   CALL      'XSRDA8'
017700000000     C                   PARM                    WLBDAT
017800000000     C     G02ERR        IFEQ      '1'
017900020308     C                   SETON                                        44  90
018000000000     C                   GOTO      ENDCTL
018100000000     C                   ENDIF
018200020307     C                   Z-ADD     G02INV        DALRIB            8 0
018300020308     C                   Z-ADD     G02dat        viddal
018400020307     c                   endif
018500020402     c                   setoff                                       47
018600030203      *
018700030203      *  Controllo Abilitazione alle tariffe
018800131007     c                   clear                   tibs42ds
018900131007     c                   movel     vidfil        i42pge
019100131007     c                   call      'TIBS42R'
019200131007     c                   parm                    TIBS42ds
019300131007     c                   movel     o42uni        uteautds
019400030227      * se abilitato con *in47 abilito la possibilità di lancio
019500030227     c                   if        vidlan ='C' and §autsdtC <> 'S'
019600020402     c                   seton                                        4790
019700030227     c                   end
019800030520      *  Per non emettere la dicitura Ristampa
019900030520     c                   if        §autCrSc = 'S'
020000030227     c                   move      'S'           autconv
020100030227     c                   end
020200000000      *
020300000000     C     ENDCTL        ENDSR
