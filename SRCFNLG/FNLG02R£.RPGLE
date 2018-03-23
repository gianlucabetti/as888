000100130109     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300100720     ffnlg02d   cf   e             workstn
000400100722     ftigcp01l  uf   e           k disk    rename(tigcp000:tigcp1)
000500100720     ftigcp02l  uf   e           k disk
000600101026     ftabel00f  if   e           k disk
000700000607     D kpjba         e ds
000800101026     D ds3k          e ds
000801171002      *ds per richiamo monitor variazioni
000802171002     d tigcp         e ds                  extname(tigcp00f)
000803171002     d fnlg33ds      e ds                  inz
000804171002     d tigcpa        e ds                  extname(tigcp00f)
000805171002     d                                     prefix(a)
000806171002     d tigcpb        e ds                  extname(tigcp00f)
000807171002     d                                     prefix(b)
000900100722     D* PARAMETRI PASSATI AL PGM DI INTERROGAZ. BOLLE FNLR36R
001000100722     D PARAM1          DS                  INZ
001100100722     D  PA1AAS                14     17  0
001200100722     D  PA1LNP                18     20  0
001300100722     D  PA1NRS                21     22  0
001400100722     D  PA1NSP                23     29  0
001500100722     D  PA1F03                30     30
001600100722     D  PA1FLG                31     31
001700000607      *--------------------------------------------------------------------
001800101026      * Casistiche valide per il ripristino delle giacenze:
001900101026      *  giacenza in fase 40
002000101026      *  giacenza in fase 50 senza bolla di recupero spese
002100101026      *  giacenza in fase 60 senza bolla di recupero spese e senza comunicazione
002200101026      *  di chiusura al cliente
002300101026      *  giacenza in fase 60 senza bolla di recupero spese e senza comunicazione
002400140527      *  di chiusura al cliente e senza VAG di chiusura
002500101026      *
002600100720     c                   do        *hival
002700100720     c                   exfmt     video1
002800100720     c                   clear                   vmsg
002900100720     c
003000100720     c   kc              leave
003100100722     c   kg              exsr      intbol
003200100722      * giacenza
003300100722     c                   if        vgiac > 0
003400100720     c     kgia          chain     tigcp02l
003500100722     c                   if        %found(tigcp02l)
003600101026     c                   movel     gcpscm        key
003700101026     c                   clear                   ds3k
003800101026     c     k3k           chain     tabel00f
003900101026     c                   if        %found(tabel00f)
004000101026     c                   movel     tbluni        ds3k
004100101026     c                   endif
004200100720     c                   select
004300100720     c                   when      gcpfas = 40  or
004400101026     c                             gcpfas = 50 and gcpnbw = 0 or
004500101026     c                             gcpfas = 60 and gcptfg = 'Z' and
004600101026     c                             (gcpnbw = 0 or gcpnbw = 9999999) or
004700101026     c                             gcpfas = 60 and gcptfg = 'N' and
004800101026     c                             (gcpnbw = 0 or gcpnbw = 9999999)
004900140527     c                             and (§3Kgqi = *blank or
005000140527     c                             §3Kgqi = 'A')
005100100721     c                   if        *inkf
005101171002     c                   eval      tigcpb = tigcp
005200100720     c                   z-add     35            gcpfas
005300100720     c                   z-add     0             gcpdbg
005400150508     c                   z-add     0             gcpdlm
005500100720     c                   update    tigcp000
005501171002     c                   exsr      RegVariazioni
005600130109     c*                  exsr      sndmail
005700100720     c                   eval      vmsg = 'Giacenza ripristinata'
005800100721     c                   endif
005900100720     c                   other
006000100720     c                   eval      vmsg = 'Giacenza non ripristinabile'
006100100720     c                   endsl
006200100720     c                   else
006300100720     c                   eval      vmsg = 'Giacenza non trovata'
006400100720     c                   endif
006500100722      *
006600100722     c                   else
006700100722      *spedizione
006800100722     c     kspe          chain     tigcp01l
006900100722     c                   if        %found(tigcp01l)
007000101026     c                   movel     gcpscm        key
007100101026     c                   clear                   ds3k
007200101026     c     k3k           chain     tabel00f
007300101026     c                   if        %found(tabel00f)
007400101026     c                   movel     tbluni        ds3k
007500101026     c                   endif
007600100722     c                   select
007700101026     c                   when      gcpfas = 40  or
007800101026     c                             gcpfas = 50 and gcpnbw = 0 or
007900101026     c                             gcpfas = 60 and gcptfg = 'Z' and
008000101026     c                             (gcpnbw = 0 or gcpnbw = 9999999) or
008100101026     c                             gcpfas = 60 and gcptfg = 'N' and
008200101026     c                             (gcpnbw = 0 or gcpnbw = 9999999)
008300140527     c                             and (§3Kgqi = *blank or
008400140527     c                             §3Kgqi = 'A')
008500100722     c                   if        *inkf
008501171002     c                   eval      tigcpb = tigcp
008600100722     c                   z-add     35            gcpfas
008700100722     c                   z-add     0             gcpdbg
008800150508     c                   z-add     0             gcpdlm
008900100722     c                   update    tigcp1
008901171002     c                   exsr      RegVariazioni
009000130109     c*                  exsr      sndmail
009100100722     c                   eval      vmsg = 'Giacenza ripristinata'
009200100722     c                   endif
009300100722     c                   other
009400100722     c                   eval      vmsg = 'Giacenza non ripristinabile'
009500100722     c                   endsl
009600100722     c                   else
009700100722     c                   eval      vmsg = 'Giacenza non trovata'
009800100722     c                   endif
009900100722     c                   endif
010000100722     c                   enddo
010100000607     c                   seton                                        lr
010200100722      *
010300100722      *-----------------------------------------------
010400100722     c     intbol        begsr
010500100722      *-----------------------------------------------
010600100722     C                   CLEAR                   param1
010700100820     C                   MOVEL     '2'           Pa1FLG
010800100820     c                   if        vgiac > 0 or vsped > 0
010900100820     c                   movel     gcpaas        PA1AAS
011000100820     c                   movel     gcplnp        PA1LNP
011100100820     c                   movel     gcpnrs        PA1NRS
011200100820     c                   movel     gcpnsp        PA1NSP
011300100820     c                   move      '1'           pa1flg
011400100820     c                   endif
011500100722     C                   MOVEL     param1        KPJBU
011600100722     C                   CALL      'FNLR36R'
011700100722     C                   PARM                    KPJBA
011800100722     c                   endsr
011900100720      *-----------------------------------------------
012000100720     c     sndmail       begsr
012100100722      *-----------------------------------------------
012200100720     C* Inizializzo variabili
012300100720     C                   movel     *blanks       wrkEml          253
012400100720     C                   movel     *blanks       wrkMsg         5000
012500100720     C                   movel     *blanks       wrkOgg           44
012600100720     C* Valorizzo i campi della e-m@ail
012700100720     C                   eval      wrkEml= 'Michele.Malipiero@bartolini.it'
012800100720
012900100720     C                   eval      wrkOgg='Ripristino Giacenza'
013000100720     C                   eval      wrkMsg='Attenzione è stata ripristinata ' +
013100100720     C                             'la giacenza numero  '+':/N'+  '-'
013200100825     C                             +  %editc(gcpfgc:'Z')
013300100825     C                             + '/ ' + %editc(gcpngc:'Z')
013400100825     C                             + '/ ' + %editc(gcpagc:'Z')
013500100825     c                             + ' dal profilo ' + knmus + 'Spedizione '
013600100825     C                             +  %editc(gcpaas:'Z')
013700100825     C                             + '/ ' + %editc(gcplnp:'Z')
013800100825     C                             + '/ ' + %editc(gcpnrs:'Z')
013900100825     C                             + '/ ' + %editc(gcpnsp:'Z')
014000100720     C                   call(e)   'TIS701C'
014100100720     C                   parm                    wrkEml
014200100720     C                   parm                    wrkOgg
014300100720     C                   parm                    wrkMsg
014400100720
014500100720     c                   endsr
014501171002     C*__________________________________________________________
014502171002     c     RegVariazioni begsr
014503171002     C*__________________________________________________________
014504171002     c                   eval      ILG33CTA = 'I'
014505171002     c                   eval      ILG33PRU = knmus
014506171002     c                   eval      ILG33NOJ = knmeb
014507171002     c                   eval      ILG33PGM = 'FNLG02R'
014508171002     c                   eval      tigcpa = tigcp
014509171002     c                   call      'FNLG33R'
014510171002     c                   parm                    fnlg33ds
014511171002     c                   parm                    tigcpb
014512171002     c                   parm                    tigcpa
014513171002     c                   endsr
014600000607      *-----------------------------------------------
014700000607     c     *inzsr        begsr
014800000607      *-----------------------------------------------
014900000607     c
015000000607     c     *entry        plist
015100000607     c                   parm                    kpjba
015200100722     c                   movel     *year         vanno
015300100722     c                   movel     *year         vannos
015400000607
015500100720     c     kgia          klist
015600100720     c                   kfld                    vanno
015700100720     c                   kfld                    vfil
015800100720     c                   kfld                    vgiac
015900100722     c     kspe          klist
016000100722     c                   kfld                    vannos
016100100722     c                   kfld                    vLNP
016200100722     c                   kfld                    vserie
016300100722     c                   kfld                    vsped
016400101026     C     K3k           KLIST
016500101026     C                   KFLD                    CODUT             1 0
016600101026     C                   KFLD                    COD               2
016700101026     C                   KFLD                    KEY               8
016800101026     C                   Z-ADD     1             CODUT
016900101026     c                   move      '3K'          cod
017000100720     c                   if        %subst(knmus:1:3) <>'EDP' and
017100100720     c                             %subst(knmus:1:2) <>'RP'
017200100720     c                   return
017300100720     c                   endif
017400100720     c                   endsr
017500000607     O*------------------------------------------------------*
