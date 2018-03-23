000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400030110      *  Descrizione   :  Conferma Valorizzazione coop
000500020624      *                   Programma di lancio
000600000000      *****************************************************************
000700030110     Fficn37D   CF   E             WORKSTN
000800000000      *
000900940926     D*
001000000000     D KPJBA         E DS
001100121024     D savpjbu         S                   like(KPJBU)
001200000000     D CNCR80        E DS
001300000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001400121024     D ficng5DS      E DS
001500020307     D dsparam         DS
001600030110     D  parpo                         3s 0
001700030110     D  data                          8s 0
001800030219     D  error                         1
001900940926     D*
002000030114     D ficn70ds      E DS
002100030114     D daut          E DS
002200940926     D*
002300000000     D WLBDAT          DS
002400940926     D  G02DAT                 1      8  0
002500940926     D  G02INV                 9     16  0
002600940926     D  G02ERR                17     17
002700940926     D  G02TGI                18     22  0
002800000000      *
002900000000     C     *ENTRY        PLIST
003000000000     C                   PARM                    KPJBA
003100000000      *
003200000000      *--------------------------------------------------------------*
003300000000      *             M A I N      L I N E
003400000000      *--------------------------------------------------------------*
003500000000     C                   Z-ADD     1             CODUT
003600000000     C                   CALL      'X§PARUT'
003700000000     C                   PARM                    UT§DSE
003800000000     C                   MOVEL     RAGUT         RSUT             20
003900000000     C                   MOVEL     RAGUT         RSUT2            20
004000000000     C                   MOVEL     REC80         CNCR80
004100000000      *
004200000000      * Inizializzo i campi a video
004300030110     C                   Z-ADD     *ZERO         VIDdat                         DATA INIZIALE
004400030110     c                   move      simpou        vsinpo
005500000000      * Emetto il formato video
005600000000     C     EMETTO        TAG
005700020308     C                   SETOFF                                       90
005800030110     C                   EXFMT     cn37D01
005900030219     c                   clear                   error
006000130808     c                   setoff                                       505166
006100030114      * forzo uscita se non autorizzato
006300000000     C   KC              GOTO      FINE                                         F3=USCITA
006400000000      *
006500000000      * Eseguo i  controlli sui campi immessi a video
006600000000     C                   EXSR      CONTR
006700000000      *
006800020308      * Se non vi sono errori ma non e' premuto F6 emetto video
006900020308     C  N90
007000020308     CanNkf              SETON                                          90      F6=CONFERMA
007100000000      *
007200000000      * Per errore generico riemetto il formato video
007300000000     C   90              GOTO      EMETTO
007400121024     C*
007500121024     C*  esegue controlli sulle FIRME
007600121024     C                   EXSR      Ctrl_FIRME
007700121024     C*  se deve essere bloccato può solo fare F3 e uscire da questo pgm
007800121024     c                   if          devo_bloccare = 'S'
007900121024     C                   GOTO      EMETTO
008000121024     c                   end
008100000000      *
008200030110      * Richiamo programma di elaborazione
008300020308     c                   if        *inkf
008400030110     c                   z-add     dalrib        data
008500030108     c                   z-add     vsinpo        parpo
008600030219     c                   move      *blank        error
008700020307     c                   movel     dsparam       kpjbu
008800030110     c                   call      'FICN37R1'
008900020624     c                   parm                    kpjba
009000030219     c                   movel     kpjbu         dsparam
009100120827     c                   if        error <> ' '
009200120827     c     error         comp      '1'                                    50
009300120827     c     error         comp      '2'                                    51
009400030219     c                   goto      emetto
009500030219     c                   endif
009600120827     c                   endif
009700000000      *
009800000000     C     FINE          TAG
009900000000     C                   SETON                                        LR
010000000000      *--------------------------------------------------------------*
010100000000      * Routine per i controlli sui campi del formato video          *
010200000000      *--------------------------------------------------------------*
010300000000     C     CONTR         BEGSR
010400000000      *
010500020308     c                   movea     '000000'      *in(41)
010600000000      *
010700020624      * controllo immissione delle date
010800030110     C     VIDDAt        ifeq      0
010900020308     C                   SETON                                        4390
011000000000     C                   GOTO      ENDCTL
011100000000     C                   END
011200030110     C     Vsinpo        ifeq      0
011300020624     C                   SETON                                        4190
011400020624     C                   GOTO      ENDCTL
011500020624     C                   END
011501130808      *controllo se utente autorizzato alla funzione
011502130808     c                   movel     simpou        i70pog
011503130808     c                   movel     vsinpo        i70pge
011504130808     c                   movel     knmus         i70prf
011505130808     c                   call      'FICN70R'
011506130808     c                   parm                    ficn70ds
011507130808     c                   movel     o70uni        daut
011508130808     c                   if        §autconc <> 'S'
011509130808     c                   seton                                        66  90
011510130808     C                   GOTO      ENDCTL
011511130808     c                   endif
011600000000      * Controllo data iniziale
011700030110     C     VIDDAt        IFGT      0
011800030110     C                   Z-ADD     VIDDAt        G02DAT
011900000000     C                   MOVEL     *BLANK        G02ERR
012000940926     C                   CALL      'XSRDA8'
012100000000     C                   PARM                    WLBDAT
012200000000     C     G02ERR        IFEQ      '1'
012300020308     C                   SETON                                        44  90
012400000000     C                   GOTO      ENDCTL
012500000000     C                   ENDIF
012600020307     C                   Z-ADD     G02INV        DALRIB            8 0
012700030110     C                   Z-ADD     G02dat        viddat
012800020307     c                   endif
012900000000      *
013000000000     C     ENDCTL        ENDSR
013100121024     C* --------------------------------------------------------------
013200121024     C*--- CONTROLLO sui DOCUMENTI Firmati della filiale -------------*
013300121024     C* --------------------------------------------------------------
013400121024     C     CTRL_FIRME    BEGSR
013500121024     C*
013600121024     C*  esegue un controllo sulle Valorizzazioni Firmate (NON solo x Autisti)
013700121024     C*   ma anche x COOP e AFFLUENZE/DEFLUENZE
013800121024     C*  Le firme devono essere a posto su tutta la filiale trattata altrimenti
013900121024     C*   viene emessa una Window di avvertimento e addirittura di BLOCCO.
014000121024     c                   move      'N'           devo_bloccare     1
014100121024     C                   MOVEL     kpjbu         savpjbu
014200121024     C*
014300121024      *  Inp-> §G5CNVL     1 A  C=CONTABILIZZAZIONE/V=VALORIZZAZIONI
014400121024      *  Inp-> §G5TIPO     1 A  BLANK=TUTTI/C=COOP/A=AUTCITTA/F=AFFDEF
014500121024      *  Inp-> §G5FIL      3 S0 FILIALE SOTTO CONTROLLO
014600121024      *  Out <-§G5FUN      2 A  TASTO FUNZIONALE PREMUTO
014700121024      *  Out <-§G5ESITO    1 A  F=FORZABILE/B=BLOCCANTE
014800121024     C*
014900121024     C                   clear                   kpjbu
015000121024     C                   clear                   ficng5ds
015100121024     c                   eval      §G5CNVL  = 'V'
015200121024     c                   eval      §G5FIL   = VSINPO
015300121024     C                   MOVEL     ficng5ds      kpjbu
015400121024     c                   call      'FICNG5R'
015500121024     C                   parm                    kpjba
015600121024     C                   MOVEL     kpjbu         ficng5ds
015700121024     C                   clear                   kpjbu
015800121024     C                   MOVEL     savpjbu       kpjbu
015900121024     C*
016000121024     C*   se deve bloccare le valorizzazioni xchè la filiale è fuori tempo max
016100121024     C*    e devono firmare prima tutti i documenti in sospeso
016200121024     C*  è proprio il FICNG5R che ci dice questo e finchè non si sono messi a
016300121024     C*  posto con le firme non fa procedere con le valorizzazioni degli AUT
016400121024     c                   if        §G5ESITO ='B'
016500121024     c                   eval        devo_bloccare = 'S'
016600121024     c                   end
016700121024     C*
016800121024     C*
016900121024     C                   ENDSR
017000121024     C* --------------------------------------------------------------
