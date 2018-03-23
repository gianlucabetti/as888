000100980721      *----------------------------------------------------*
000200980721      * Annullamento / Ripristino C.A.
000300980721      *----------------------------------------------------*
000400980721     Ffndcc01l  UF   E           K DISK
000500980721     Ffndcr01l  UF   E           K DISK
000600980721     Ffndcs01l  UF   E           K DISK
000700980721     Ffndcd01l  UF   E           K DISK
000800981022     Ffndcl01l  UF   E           K DISK
000900980721     Ffndka01l  UF   E           K DISK
001000040722     FFndck01l  UF   E           K DISK
001100980721     Ffndcf01l  UF   E           K DISK
001200990408     Ffndpt01l  UF   E           K DISK
001300980721     Ffndct01l  UF   E           K DISK
001400980721
001500980721     D MSG             S             78    DIM(1) CTDATA PERRCD(1)              MSG ERRORI
001600980721     D ktpd            s                   LIKE(DCStpd)  INZ('C')
001700980721     D knkt            s                   LIKE(DCSnkt)
001800980721     D watb            s                   LIKE(DCTatb)
001900000000     D KPJBA         E DS
002000980721     D fidn11ds      E DS
002100980721     d  dsnumca                2     15
002200980721
002300980721     c                   IF        I11mod <> 'A'  and  I11mod <> 'R'
002400990820     c                                            and  I11mod <> 'D'
002500990908     c                                            and  I11mod <> 'F'
002600980721     c                   movel     'E'           O11err
002700980721     c                   movel     MSG(1)        O11msg
002800980721     C                   ELSE
002900980721
003000980721      * FNDCF
003100980721     c     knca          setll     fndcf000                               30
003200980721     c   30              DO        *hival
003300980721     c     knca          reade     fndcf000                               30
003400980721     c                   if        *in30 = *OFF
003500990908     c  n02              if        i11mod='F'
003600990908     c                   clear                   dcfftr
003700990908     c                   else
003800990908     c                   movel     watb          DCFatb
003900990908     c                   clear                   DCFftr
004000990908     c                   endif
004100990908     c
004200990820     c  n02              update    fndcf000
004300990820     c   02              delete    fndcf000
004400980721     c                   endif
004500980721     c  N30              ENDDO
004600980721
004700980721      * FNDCD
004800980721     c     knca          setll     fndcd000                               30
004900980721     c   30              DO        *hival
005000980721     c     knca          reade     fndcd000                               30
005100980721     c                   if        *in30 = *OFF
005200990908     c  n02              if        i11mod='F'
005300990908     c                   clear                   DCDftr
005400990908     c                   else
005500990908     c                   movel     watb          DCDatb
005600990908     c                   clear                   DCDftr
005700990908     c                   endif
005800990908     c
005900990820     c  n02              update    fndcd000
006000990820     c   02              delete    fndcd000
006100980721     c                   endif
006200980721     c  N30              ENDDO
006300980721
006400980721      * FNDCR
006500980721     c     knca          setll     fndcr000                               30
006600980721     c   30              DO        *hival
006700980721     c     knca          reade     fndcr000                               30
006800980721     c                   if        *in30 = *OFF
006900990908     c  n02              if        i11mod='F'
007000990908     c                   clear                   DCRftr
007100990908     c                   else
007200990908     c                   movel     watb          DCRatb
007300990908     c                   clear                   DCRftr
007400990908     c                   endif
007500990908     c
007600990820     c  n02              update    fndcr000
007700990820     c   02              delete    fndcr000
007800980721     c                   endif
007900980721     c  N30              ENDDO
008000980721
008100980721      * FNDCC
008200980721     c     knca          setll     fndcc000                               30
008300980721     c   30              DO        *hival
008400980721     c     knca          reade     fndcc000                               30
008500980721     c                   if        *in30 = *OFF
008600990908     c  n02              if        i11mod='F'
008700990908     c                   clear                   DCCftr
008800990908     c                   else
008900990908     c                   movel     watb          DCCatb
009000990908     c                   clear                   DCCftr
009100990908     c                   endif
009200990908     c
009300990820     c  n02              update    fndcc000
009400990820     c   02              delete    fndcc000
009500980721     c                   endif
009600980721     c  N30              ENDDO
009700980721
009800980721      * FNDCS
009900980721     c     kdcs          setll     fndcs000                               30
010000980721     c   30              DO        *hival
010100980721     c     kdcs          reade     fndcs000                               30
010200980721     c                   if        *in30 = *OFF
010300990908     c  n02              if        i11mod='F'
010400990908     c                   clear                   DCSft1
010500990908     c                   clear                   DCSdt1
010600990908     c                   else
010700990908     c                   movel     watb          DCSatb
010800990908     c                   clear                   DCSft1
010900990908     c                   clear                   DCSdt1
011000990908     c                   endif
011100990908     c
011200990820     c  n02              update    fndcs000
011300990820     c   02              delete    fndcs000
011400980721     c                   endif
011500980721     c  N30              ENDDO
011600990408
011700981022      * FNDKA
011800981022     c     knca          setll     fndka000                               30
011900981022     c   30              DO        *hival
012000981022     c     knca          reade     fndka000                               30
012100981022     c                   if        *in30 = *OFF
012200990908     c  n02              if        i11mod='F'
012300990908     c                   clear                   Dkaftr
012400990908     c                   else
012500990908     c                   movel     watb          Dkaatb
012600990908     c                   clear                   Dkaftr
012700990908     c                   endif
012800990908     c
012900990820     c  n02              update    fndka000
013000990820     c   02              delete    fndka000
013100981022     c                   endif
013200981022     c  N30              ENDDO
013300990408
013400040722      * FNDCK
013500040722     c     knca          setll     fndCK000                               30
013600040722     c   30              DO        *hival
013700040722     c     knca          reade     fndck000                               30
013800040722     c                   if        *in30 = *OFF
013900040722     c  n02              if        i11mod='F'
014000040722     c                   clear                   Dckftr
014100040722     c                   else
014200040722     c                   movel     watb          Dckatb
014300040722     c                   clear                   Dckftr
014400040722     c                   endif
014500040722     c
014600040722     c  n02              update    fndck000
014700040722     c   02              delete    fndck000
014800040722     c                   endif
014900040722     c  N30              ENDDO
015000040722
015100981022      * FNDCL
015200981022     c     knca          chain     fndcl000                           30
015300980721     c                   if        *in30 = *OFF
015400990908     c  n02              if        i11mod='F'
015500990908     c                   clear                   DCLftr
015600990908     c                   else
015700990908     c                   movel     watb          DCLatb
015800990908     c                   clear                   DCLftr
015900990908     c                   endif
016000990908     c
016100990820     c  n02              update    fndcl000
016200990820     c   02              delete    fndcl000
016300980721     c                   endif
016400990408
016500990408      * FNDPT
016600990408     c     knca          chain     fndpt000                           30
016700990408     c                   if        *in30 = *OFF
016800990908     c  n02              if        i11mod='F'
016900990908     c                   clear                   DPTftr
017000990908     c                   else
017100990908     c                   movel     watb          DPTatb
017200990908     c                   clear                   DPTftr
017300990908     c                   endif
017400990908     c
017500990820     c  n02              update    fndpt000
017600990820     c   02              delete    fndpt000
017700990408     c                   endif
017800980721
017900980721      * FNDCT
018000980721     c     knca          chain     fndct000                           30
018100980721     c                   if        *in30 = *OFF
018200990908     c  n02              if        i11mod='F'
018300990908     c                   clear                   DCTft1
018400990908     c                   clear                   DCTft2
018500990908     c                   else
018600990908     c                   movel     watb          DCTatb
018700990908     c                   clear                   DCTft1
018800990908     c                   clear                   DCTft2
018900981007     c* se ripristino una CA annullata pulisco la causale di chiusura  se dta chiusura = a 0
019000981007     c* questo perchè in fase di ricezione in arrivo di una CA aperta in partenza per una spedizione
019100981007     c* già presente in un'altra CA dell'arrivo annullo la testata CA dell'arrivo e valorizzo la
019200981007     c* causale di chiusura con RD.... ma quando viene ripristinata bisogna pulire il campo causale
019300990908     c                   if        watb = ' '  and  dctdch = 0
019400990820     c                   clear                   DCTcch
019500990820     c                   endif
019600990908     c                   endif
019700990908     c
019800990820     c  n02              update    fndct000
019900990820     c   02              delete    fndct000
020000980721     c                   endif
020100980721
020200980721     c                   ENDIF
020300980721
020400980721     c                   EVAL      *INLR = *ON
020500980721
020600980721      *-------------------------------------------------------------------------*
020700980721      * Subroutine iniziale
020800980721      *-------------------------------------------------------------------------*
020900980721     c     *inzsr        BEGSR
021000980721      *
021100980721     C     *ENTRY        PLIST
021200980721     C                   PARM                    KPJBA
021300980721     c                   parm                    fidn11ds
021400980721      *
021500980721     c                   IF        I11MOD = 'A'
021600980721     c                   movel     'A'           Watb
021700980721     c                   ENDIF
021800980721
021900980721     c                   IF        I11MOD = 'R'
022000980721     c                   movel     ' '           Watb
022100980721     c                   ENDIF
022200990820      *
022300990820     c                   IF        I11MOD = 'D'
022400990820     c                   seton                                        02
022500990820     c                   ENDIF
022600980721      *
022700980721     c                   movel(p)  dsnumca       knkt
022800980721      *
022900980721     c     KNCA          klist
023000980721     c                   kfld                    I11aac
023100980721     c                   kfld                    I11fil
023200980721     c                   kfld                    I11nca
023300980721      *
023400980721     C     KDCS          KLIST
023500980721     C                   KFLD                    KTPD
023600980721     C                   KFLD                    KNKT
023700980721      *
023800980721     c                   endsr
023900980721      *-------------------------------------------------------------------------*
024000980721**  MSG lung. 78                                                             *
024100980721Modalità richiamo programma errata: telefonare in Sede !!!
