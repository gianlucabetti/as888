000100050218     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200980721      *----------------------------------------------------*
000300980721      * Annullamento / Ripristino C.A.
000400050218      *
000500050218      * ATTENZIONE il vecchio tipo richiamo "D" ora aggiorna dctdt2
000600050218      *            la funzione di delete viene fatta con "*"
000700980721      *----------------------------------------------------*
000800980721     Ffndcc01l  UF   E           K DISK
000900980721     Ffndcr01l  UF   E           K DISK
001000980721     Ffndcs01l  UF   E           K DISK
001100980721     Ffndcd01l  UF   E           K DISK
001200981022     Ffndcl01l  UF   E           K DISK
001300980721     Ffndka01l  UF   E           K DISK
001400040722     FFndck01l  UF   E           K DISK
001500980721     Ffndcf01l  UF   E           K DISK
001600990408     Ffndpt01l  UF   E           K DISK
001700980721     Ffndct01l  UF   E           K DISK
001800980721
001900980721     D MSG             S             78    DIM(1) CTDATA PERRCD(1)              MSG ERRORI
002000980721     D ktpd            s                   LIKE(DCStpd)  INZ('C')
002100980721     D knkt            s                   LIKE(DCSnkt)
002200980721     D watb            s                   LIKE(DCTatb)
002300050218     D wdt2            s                   LIKE(DCTdt2)
002400000000     D KPJBA         E DS
002500980721     D fidn11ds      E DS
002600980721     d  dsnumca                2     15
002700050222     d fidn58ds      e ds
002800980721
002900980721     c                   IF        I11mod <> 'A'  and  I11mod <> 'R'
003000990820     c                                            and  I11mod <> 'D'
003100990908     c                                            and  I11mod <> 'F'
003200050218     c                                            and  I11mod <> '*'
003300980721     c                   movel     'E'           O11err
003400980721     c                   movel     MSG(1)        O11msg
003500980721     C                   ELSE
003600980721
003700980721      * FNDCF
003800980721     c     knca          setll     fndcf000                               30
003900980721     c   30              DO        *hival
004000980721     c     knca          reade     fndcf000                               30
004100980721     c                   if        *in30 = *OFF
004200990908     c  n02              if        i11mod='F'
004300990908     c                   clear                   dcfftr
004400990908     c                   else
004500990908     c                   movel     watb          DCFatb
004600990908     c                   clear                   DCFftr
004700990908     c                   endif
004800990908     c
004900990820     c  n02              update    fndcf000
005000990820     c   02              delete    fndcf000
005100980721     c                   endif
005200980721     c  N30              ENDDO
005300980721
005400980721      * FNDCD
005500980721     c     knca          setll     fndcd000                               30
005600980721     c   30              DO        *hival
005700980721     c     knca          reade     fndcd000                               30
005800980721     c                   if        *in30 = *OFF
005900990908     c  n02              if        i11mod='F'
006000990908     c                   clear                   DCDftr
006100990908     c                   else
006200990908     c                   movel     watb          DCDatb
006300990908     c                   clear                   DCDftr
006400990908     c                   endif
006500990908     c
006600990820     c  n02              update    fndcd000
006700990820     c   02              delete    fndcd000
006800980721     c                   endif
006900980721     c  N30              ENDDO
007000980721
007100980721      * FNDCR
007200980721     c     knca          setll     fndcr000                               30
007300980721     c   30              DO        *hival
007400980721     c     knca          reade     fndcr000                               30
007500980721     c                   if        *in30 = *OFF
007600990908     c  n02              if        i11mod='F'
007700990908     c                   clear                   DCRftr
007800990908     c                   else
007900990908     c                   movel     watb          DCRatb
008000990908     c                   clear                   DCRftr
008100990908     c                   endif
008200990908     c
008300990820     c  n02              update    fndcr000
008400990820     c   02              delete    fndcr000
008500980721     c                   endif
008600980721     c  N30              ENDDO
008700980721
008800980721      * FNDCC
008900980721     c     knca          setll     fndcc000                               30
009000980721     c   30              DO        *hival
009100980721     c     knca          reade     fndcc000                               30
009200980721     c                   if        *in30 = *OFF
009300990908     c  n02              if        i11mod='F'
009400990908     c                   clear                   DCCftr
009500990908     c                   else
009600990908     c                   movel     watb          DCCatb
009700990908     c                   clear                   DCCftr
009800990908     c                   endif
009900990908     c
010000990820     c  n02              update    fndcc000
010100990820     c   02              delete    fndcc000
010200980721     c                   endif
010300980721     c  N30              ENDDO
010400980721
010500980721      * FNDCS
010600980721     c     kdcs          setll     fndcs000                               30
010700980721     c   30              DO        *hival
010800980721     c     kdcs          reade     fndcs000                               30
010900980721     c                   if        *in30 = *OFF
011000990908     c  n02              if        i11mod='F'
011100990908     c                   clear                   DCSft1
011200990908     c                   clear                   DCSdt1
011300990908     c                   else
011400990908     c                   movel     watb          DCSatb
011500990908     c                   clear                   DCSft1
011600990908     c                   clear                   DCSdt1
011700990908     c                   endif
011800990908     c
011900990820     c  n02              update    fndcs000
012000990820     c   02              delete    fndcs000
012100980721     c                   endif
012200980721     c  N30              ENDDO
012300990408
012400981022      * FNDKA
012500981022     c     knca          setll     fndka000                               30
012600981022     c   30              DO        *hival
012700981022     c     knca          reade     fndka000                               30
012800981022     c                   if        *in30 = *OFF
012900990908     c  n02              if        i11mod='F'
013000990908     c                   clear                   Dkaftr
013100990908     c                   else
013200990908     c                   movel     watb          Dkaatb
013300990908     c                   clear                   Dkaftr
013400990908     c                   endif
013500990908     c
013600990820     c  n02              update    fndka000
013700990820     c   02              delete    fndka000
013800981022     c                   endif
013900981022     c  N30              ENDDO
014000990408
014100040722      * FNDCK
014200040722     c     knca          setll     fndCK000                               30
014300040722     c   30              DO        *hival
014400040722     c     knca          reade     fndck000                               30
014500040722     c                   if        *in30 = *OFF
014600040722     c  n02              if        i11mod='F'
014700040722     c                   clear                   Dckftr
014800040722     c                   else
014900040722     c                   movel     watb          Dckatb
015000040722     c                   clear                   Dckftr
015100040722     c                   endif
015200040722     c
015300040722     c  n02              update    fndck000
015400040722     c   02              delete    fndck000
015500040722     c                   endif
015600040722     c  N30              ENDDO
015700040722
015800981022      * FNDCL
015900981022     c     knca          chain     fndcl000                           30
016000980721     c                   if        *in30 = *OFF
016100990908     c  n02              if        i11mod='F'
016200990908     c                   clear                   DCLftr
016300990908     c                   else
016400990908     c                   movel     watb          DCLatb
016500990908     c                   clear                   DCLftr
016600990908     c                   endif
016700990908     c
016800990820     c  n02              update    fndcl000
016900990820     c   02              delete    fndcl000
017000980721     c                   endif
017100990408
017200990408      * FNDPT
017300990408     c     knca          chain     fndpt000                           30
017400990408     c                   if        *in30 = *OFF
017500990908     c  n02              if        i11mod='F'
017600990908     c                   clear                   DPTftr
017700990908     c                   else
017800990908     c                   movel     watb          DPTatb
017900990908     c                   clear                   DPTftr
018000990908     c                   endif
018100990908     c
018200990820     c  n02              update    fndpt000
018300990820     c   02              delete    fndpt000
018400990408     c                   endif
018500980721
018600980721      * FNDCT
018700980721     c     knca          chain     fndct000                           30
018800980721     c                   if        *in30 = *OFF
018900990908     c  n02              if        i11mod='F'
019000990908     c                   clear                   DCTft1
019100990908     c                   clear                   DCTft2
019200990908     c                   else
019300990908     c                   movel     watb          DCTatb
019400990908     c                   clear                   DCTft1
019500990908     c                   clear                   DCTft2
019600981007     c* se ripristino una CA annullata pulisco la causale di chiusura  se dta chiusura = a 0
019700981007     c* questo perchè in fase di ricezione in arrivo di una CA aperta in partenza per una spedizione
019800981007     c* già presente in un'altra CA dell'arrivo annullo la testata CA dell'arrivo e valorizzo la
019900981007     c* causale di chiusura con RD.... ma quando viene ripristinata bisogna pulire il campo causale
020000990908     c                   if        watb = ' '  and  dctdch = 0
020100990820     c                   clear                   DCTcch
020200990820     c                   endif
020300990908     c                   endif
020400050218     c                   If        i11mod = 'D'
020500050218     c                   Eval      dctdt2 = wdt2
020600050218     c                   endif
020700990908     c
020800990820     c  n02              update    fndct000
020900990820     c   02              delete    fndct000
021000980721     c                   endif
021100050222
021200050222      * Richiamo FIDN58R x aggiornare titas
021300050222     c                   Clear                   Fidn58ds
021400050222     c                   Eval      i58aas = dctaas
021500050222     c                   Eval      i58lnp = dctlnp
021600050222     c                   Eval      i58nrs = dctnrs
021700050222     c                   Eval      i58nsp = dctnsp
021800050222     c                   movel     fidn58ds      kpjbu
021900050222     c                   call      'FIDN58R'
022000050222     c                   parm                    kpjba
022100980721
022200980721     c                   ENDIF
022300980721
022400980721     c                   EVAL      *INLR = *ON
022500980721
022600980721      *-------------------------------------------------------------------------*
022700980721      * Subroutine iniziale
022800980721      *-------------------------------------------------------------------------*
022900980721     c     *inzsr        BEGSR
023000980721      *
023100980721     C     *ENTRY        PLIST
023200980721     C                   PARM                    KPJBA
023300980721     c                   parm                    fidn11ds
023400980721      *
023500980721     c                   IF        I11MOD = 'A'
023600980721     c                   movel     'A'           Watb
023700980721     c                   ENDIF
023800980721
023900980721     c                   IF        I11MOD = 'R'
024000980721     c                   movel     ' '           Watb
024100980721     c                   ENDIF
024200050218
024300050218     c                   IF        I11MOD = 'D'
024400050218     c                   Eval      wdt2 = *date
024500050218     c                   ENDIF
024600990820      *
024700050218     c**!!!              IF        I11MOD = 'D'
024800050218     c                   IF        I11MOD = '*'
024900990820     c                   seton                                        02
025000990820     c                   ENDIF
025100980721      *
025200980721     c                   movel(p)  dsnumca       knkt
025300980721      *
025400980721     c     KNCA          klist
025500980721     c                   kfld                    I11aac
025600980721     c                   kfld                    I11fil
025700980721     c                   kfld                    I11nca
025800980721      *
025900980721     C     KDCS          KLIST
026000980721     C                   KFLD                    KTPD
026100980721     C                   KFLD                    KNKT
026200980721      *
026300980721     c                   endsr
026400980721      *-------------------------------------------------------------------------*
026500980721**  MSG lung. 78                                                             *
026600980721Modalità richiamo programma errata: telefonare in Sede !!!
