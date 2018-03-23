000100990127      *----------------------------------------------------*
000200990127      *  Stampa Sollecito Avvisi Danno mai pervenuti
000300990127      *----------------------------------------------------*
000400990127
000500990127      ****************************************************************
000600990127      *  FASI C.A. FISSATE A PROGRAMMA
000700990127      ****************************************************************
000800990127      *  575 Sollecito Avvisi Danni mai Pervenuti   wfaso
000900990208      *  545 Stampa avviso danno                    wfaad
001000990208      *  550 Modifica rivalsa                       wfamr
001100990127      ***************************************************************
001200990127
001300990127      ****************************************************************
001400990127      *  RIEPILOGO INDICATORI
001500990127      ***************************************************************
001600990127      * 30 - COMODO
001700990127      * 31 - COMODO
001800990127      * 32 - COMODO
001900990127      ***************************************************************
002000990127
002100990127     H DECEDIT('0,') DATEDIT(*DMY.)
002200990127
002300990127     FFNDCT01L  IF   E           K DISK
002400990127     FFNDCF01L  IF   E           K DISK
002500990208     FFNDfa01L  IF   E           K DISK
002600990127
002700990127      *------------------------------------------------------------------------*
002800990202     D kaac            s                   like(dctaac)
002900990202     D kfil            s                   like(dctfil)
003000990202     D knca            s                   like(dctnca)
003100990202     d Wfaso           s                   like(dctfca) inz(575)
003200990208     d Wfaad           s                   like(dctfca) inz(545)
003300990208     d Wfamr           s                   like(dctfca) inz(550)
003400990208     D Wdataeur        S               D   DATFMT(*eur)
003500990208     D Wdataoggi       S               D   DATFMT(*eur)
003600990202     D dateu           s              8  0
003700990202     D wdtgio          S              8  0
003800990202     D w0140           S             14  0
003900990202      *
004000990202     D fidn00ds      E DS
004100990202      *
004200990202     D KPJBA         E DS
004300990208      *
004400990208     D WLBDAT          DS                  INZ
004500990208     D  G02DAT                 1      8  0
004600990208     D  G02INV                 9     16  0
004700990208     D  G02ERR                17     17
004800990208     D  G02TGI                18     22  0
004900010406     D param           DS                  inz
005000010406     d  datadal                       8  0
005100010406     d  dataall                       8  0
005200010406      *
005300990202      *****************************************************************
005400990202      *
005500990202     c* Lettura di tutte le CA
005600990202      *
005700990202     c                   do        *hival
005800990202      *
005900990202     c                   read      fndct01l                               30
006000990202      * se CA non annullata e non chiusa e con fase minore di quella da fare
006100990202     c                   if        not *in30       and
006200990202     c                             dctatb = ' '    and
006300990208     c                             dctdch = 0      and
006400990208     c                             (dctfca = wfaad or dctfca = wfamr)
006500990202      * verifico se è nella fase giusta per stampare il sollecitone
006600990202     c                   exsr      SUB_CTR
006700990202      * se CA non regolare leggo la successiva
006800990202     c                   if        *in31
006900990202     c                   iter
007000990202      *
007100990202     c                   else
007200990202      * se CA regolare stampo il sollecito
007300990202     c                   exsr      SUB_SOL
007400990202      *
007500990202     c                   endif
007600990202      *
007700990202     c                   endif
007800990202      *
007900990202     c  N30              enddo
008000990202      *
008100990202     c                   eval      *inlr = *on
008200990202      *
008300990202      *---------------------------------------------------------------*
008400990202      *   SUB_CTR   Routine controllo CA
008500990202      *---------------------------------------------------------------*
008600990202     C     SUB_CTR       BEGSR
008700990202      *
008800990202     c                   setoff                                           31
008900990202      *  per fase esistente recupero la data di esecuzione fase
009000990202     c                   z-add     DCTaac        Kaac
009100990202     c                   z-add     DCTfil        Kfil
009200990202     c                   z-add     DCTnca        Knca
009300990202     c     kdcf          setgt     fndcf000
009400990202     c     kdcf          readpe    fndcf000                               32
009500990202     c                   if        *in32 = *off and dcffca = dctfca
009600990208     c* controllo le date
009700010406     c* verifico se la data della fase è compresa nei limiti di date inserite a video
009800010406     c*
009900041110     c                   if        dcfdfc >= datadal and
010000041110     c                             dcfdfc <= dataall
010100010406     c* controllo i giorni
010200990208     c     *iso          move      dcfdfc        Wdataeur
010300990208     c     wdataoggi     subdur    wdataeur      num_gior:*D       5 0
010400990208     c                   if        num_gior <  dfagga
010500990202     c                   seton                                        31
010600990202     c                   endif
010700010411      *
010800010411      * se la data non è compresa tra i limiti accendo il 31 per non eseguire la CA
010900010411      *
011000010411     c                   else
011100010411     c                   seton                                        31
011200990208     c*
011300010406     c                   endif
011400010406     c*
011500990208     c                   endif
011600990202      *
011700990202     c                   endsr
011800990202      *---------------------------------------------------------------*
011900990202      *   SUB_SOL   Routine richiamo PGM Stampa solleciti
012000990202      *---------------------------------------------------------------*
012100990202     C     SUB_SOL       BEGSR
012200990202      *
012300990202      * preparo la ds
012400990202     c                   clear                   fidn00ds
012500990202     c*
012600990202     c                   eval      i00fca = wfaso
012700990202     c                   eval      i00fgs = dctgfc
012800990202     c                   eval      i00aac = dctaac
012900990202     c                   eval      i00fil = dctfil
013000990202     c                   eval      i00nca = dctnca
013100990202     c                   eval      i00aas = dctaas
013200990202     c                   eval      i00lnp = dctlnp
013300990202     c                   eval      i00nrs = dctnrs
013400990202     c                   eval      i00nsp = dctnsp
013500990202     c                   eval      i00tad = dcttad
013600990202      *
013700990202     c                   movel     fidn00ds      kpjbu
013800990202     c                   call      'FIDN28R'
013900990202     c                   parm                    kpjba
014000990202      *
014100990202      *
014200990202     c                   endsr
014300990127      *****************************************************************
014400990127      *   ROUTINE INIZIALE
014500990127      *****************************************************************
014600990127     C     *INZSR        BEGSR
014700990127      *
014800990127     C     *ENTRY        PLIST
014900990127     C                   PARM                    KPJBA
015000010406     C                   movel     kpjbu         param
015100990208      * ORA
015200990208     C                   TIME                    W0140
015300990208      * UDATE IN GGMMAAAA
015400990208     C                   MOVE      W0140         WDTGIO
015500990208      * UDATE IN AAAAMMGG
015600990208     C                   Z-ADD     WDTGIO        G02DAT
015700990208     C                   MOVEL     *BLANK        G02ERR
015800990208     C                   CALL      'XSRDA8'
015900990208     C                   PARM                    WLBDAT
016000990208     C                   MOVEL     G02INV        DATEU
016100990208     C     *iso          MOVEL     DATEU         Wdataoggi
016200990127      *
016300990127      * accesso testata CA
016400990127     C     kdcf          KLIST
016500990202     C                   KFLD                    kaac
016600990202     C                   KFLD                    kfil
016700990202     C                   KFLD                    knca
016800990208      *
016900990208      * recupero i giorni di attesa da l'ultima fase
017000990208      *
017100990208     c     wfaso         chain     fndfa01l                           30
017200990208      *
017300990127     C                   ENDSR
