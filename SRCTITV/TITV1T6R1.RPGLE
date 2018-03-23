000100090114      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500091216     FTIVGD00F  O    E             DISK
000600990908
000700000801     D*----------------------------------------------------
000800000801     D* DICHIARAZIOINE VARIABILI DI WRK
000900000801     D*----------------------------------------------------
001000990910     D psds           sds
001100990910     D  procname         *PROC
001200990920     D tivlrds       e ds                  extname(tivlr00f)
001300091216     D fnvac00t      e ds
001400990910     D esito           s              1
001500091216     D errore          s              6  0 inz
001600000724     D prmlit          s             10
001700000710     D prmfir          s             10
001800990921     D wrkesito        s                   like(esito)
001900990915     D wrkdata         s               d
002000990915     D wrkora          s               t
002100100118     D wContaRecord    s              2  0 inz
002200091216
002300091216
002400091216     D*---------------------------------------------------------
002500091216     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
002600091216     D*---------------------------------------------------------
002700091216     D                 ds
002800091216     D vhbatb                         1
002900091216     D vhbccm                         7
003000091216     D vhblnp                         3
003100091216     D vhbaas                         4
003200091216     D vhbmgs                         4
003300091216     D vhbnrs                         2
003400091216     D vhbnsp                         7
003500091216     D vhbcbo                         2
003600091216     D vhblna                         3
003700091216     D vhbrsd                        35
003800091216     D vhbrd2                        35
003900091216     D vhbind                        35
004000091216     D vhbcad                         9
004100091216     D vhblod                        35
004200091216     D vhbprd                         2
004300091216     D vhbnzd                         3
004400091216     D vhbgc1                         2
004500091216     D vhbgc2                         2
004600091216     D vhbctr                         3
004700091216     D vhbtsp                         1
004800091216     D vhbias                        13
004900091216     D vhbvas                         3
005000091216     D vhbnas                        15
005100091216     D vhbncl                         5
005200091216     D vhbpkb                         7
005300091216     D vhbvlb                         5
005400091216     D vhbqft                        13
005500091216     D vhbcas                        13
005600091216     D vhbtic                         2
005700091216     D vhbvca                         3
005800091216     D vhbgca                         2
005900091216     D vhbrmn                        15
006000091216     D vhbrma                        15
006100091216     D vhbncd                         7
006200091216     D vhbnca                         7
006300091216     D vhbxco                         1
006400091216     D vhbnot                        35
006500091216     D vhbnt2                        35
006600091216     D vhbznc                         2
006700091216     D vhbctm                         2
006800091216     D vhbffd                         1
006900091216     D vhbdcr                         8
007000091216     D vhbtcr                         1
007100091216     D vhbhcr                         4
007200091216     D vhbcts                         2
007300091216     D vhbftm                         1
007400091216     D vhbvmd                        13
007500091216     D vhbvad                         3
007600091216     D vhbgma                         2
007700091216     D vhbgga                         2
007800091216     D vhbgva                         2
007900091216     D vhbtc1                         1
008000091216     D vhbtc2                         1
008100091216     D vhbscl                         1
008200091216     D vhbant                         9
008300091216     D vhbrmo                        25
008400091216     D vhbcmo                         9
008500091216     D vhbnmo                         3
008600091216     D  X_rec                  1    486    inz
008700091216     D*
008800090114
008900091215     D*------------------
009000091215     D* DS REPERIMENTO NUMERATORE
009100091215     D*------------------
009200091215     D trul33ds      e ds                  inz
009300091215     D*------------------
009400091215     D* DS ARCHITETTURA
009500091215     D*------------------
009600091215     D kpjba         e ds                  inz
009700091215
009800081217
009900010201
010000081217
010100081217     C
010200990921     C                   reset                   wrkesito
010300000613     C*
010400040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010500000616     C*
010600000801     C
010700010201     C                   seton                                        LR
010800990908
010900000801
011000910830     C*--------------------------------------------------------
011100090114     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
011200910830     C*--------------------------------------------------------
011300040526     C     RWFILE        BEGSR
011400990910     C*
011500990914     C                   if        not %open(tivin00r)
011600990908     C                   open      tivin00r
011700990914     C                   endif
011800990910     C*
011900100118     C                   eval      wContaRecord = 0
012000100118     C*
012100921023     C                   DO        *HIVAL
012200990913     C*
012300990915     C                   READ      tivin00r                               70
012400050627     C                   if        vindta > *blanks
012500000801     C*
012600091214     C                   if        *in70 = *off
012700000801     C                             and
012800000801     C                             (vinflg = *blanks
012900000801     C                              or vinflg = '0'
013000000801     C                              or vinflg = '2')
013100100118     C*
013200100118     C                   eval      wContaRecord = wContaRecord + 1
013300000801     C*
013400000801     C                   clear                   vinmsg
013500000801     C                   eval      vinflg = '1'
013600091216     C                   exsr      inzvar
013700091216     C                   exsr      impvab
013800091216     C                   exsr      repnsp
013900091216     C                   exsr      wrivgd
014000000905     C*
014100000905     C                   else
014200000905     C                   eval      vinflg = '1'
014300050628     C                   endif
014400000905     C                   endif
014500000905     C*
014600000905     C  N70              update    tivin000
014700000905     C*
014800991022     C  N70              ENDdo
014900010202     C*
015000990910
015100091216     C*
015200091216     C                   if        errore > *zeros
015300091216     C                   eval      wrkesito = '6'
015400091216     C                   else
015500091216     C                   eval      wrkesito = '0'
015600091216     C                   endif
015700091216     C*
015800990914     C                   if        %open(tivin00r)
015900990908     C                   close     tivin00r
016000990914     C                   endif
016100990920     C*
016200910830     C                   ENDSR
016300000613     C***
016400990920
016500000801     C*----------------------------------------------------*
016600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
016700000801     C*----------------------------------------------------*
016800010201     C     INZVAR        BEGSR
016900000801     C*
017000091216     C                   CLEAR                   FNVAC00T
017100000801     C*
017200000801     C                   ENDSR
017300091215     C*----------------------------------------------------*
017400091215     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
017500091215     C*----------------------------------------------------*
017600091215     C     REPNSP        BEGSR
017700091215     C*
017800091215     C* NSP => Stacco un numeratore da AZNUM
017900091215     C                   clear                   TRUL33DS
018000091215     C                   eval      I33OPE = *zeros
018100091215     C                   eval      I33CNU = 302
018200091215     C                   eval      I33NUM = 1
018300091215     C                   movel     TRUL33DS      KPJBU
018400091215     C                   call      'TRUL33R'
018500091215     C                   parm                    KPJBA
018600091215     C                   movel     KPJBU         TRUL33DS
018700091215     C                   if        O33ERR = *zeros
018800091216     C                   z-add     O33NRF        VABNSP            7 0
018900091215     C                   else
019000091215     C                   ADD       1             errore
019100091215     C                   EVAL      vinmsg = %trimr(vinmsg)
019200091216     C                             + ' ' + 'VABNSP'
019300091215     C                   endif
019400091215     C*
019500091216     C                   EXSR      DEFCAM
019600091216     C*
019700091215     C                   ENDSR
019800000801     C*----------------------------------------------------*
019900090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
020000000801     C*----------------------------------------------------*
020100040823     C     IMPVAB        BEGSR
020200091216     C*
020300091216     C                   eval      x_rec  = vindta
020400091216     C*
020500091216     C* ...numerici
020600091216     C                   move(p)   vhbAAS        VACAAS
020700091216     C                   move(p)   vhbLNP        VACLNP
020800091216     C                   move(p)   vhbNRS        VACNRS
020900091216     C                   move(p)   vhbMGS        VACMGS
021000091216     C                   move(p)   vhbCTR        VACCTR
021100091216     C                   move(p)   vhbIAS        VACIAS
021200091216     C                   move(p)   vhbNCL        VACNCL
021300091216     C                   move(p)   vhbPKB        VACPKB
021400091216     C                   move(p)   vhbVLB        VACVLB
021500091216     C                   move(p)   vhbQFT        VACQFT
021600091216     C                   move(p)   vhbCAS        VACCAS
021700091216     C                   move(p)   vhbRMN        VACRMN
021800091216     C                   move(p)   vhbDCR        VACDCR
021900091216     C                   move(p)   vhbHCR        VACHCR
022000091216     C                   move(p)   vhbCCM        VACCCM
022100091216     C* ...alfabetici
022200091216     C                   eval      VACCBO = vhbCBO
022300091216     C                   eval      VACRSD = vhbRSD
022400091216     C                   eval      VACPRD = vhbPRD
022500091216     C                   eval      VACGC1 = vhbGC1
022600091216     C                   eval      VACGC2 = vhbGC2
022700091216     C                   eval      VACCTS = vhbCTS
022800091216     C                   eval      VACFTM = vhbFTM
022900091216     C                   eval      VACTSP = vhbTSP
023000091216     C                   eval      VACVAS = vhbVAS
023100091216     C                   eval      VACNAS = vhbNAS
023200091216     C                   eval      VACTIC = vhbTIC
023300091216     C                   eval      VACVCA = vhbVCA
023400091216     C                   eval      VACRMA = vhbRMA
023500091216     C                   eval      VACRMO = vhbRMO
023600091216     C                   eval      VACFFD = vhbFFD
023700091216     C                   eval      VACTCR = vhbTCR
023800091216     C                   eval      VACTC1 = vhbTC1
023900091216     C                   eval      VACTC2 = vhbTC2
024000091216     C* ...inizializzati
024100091216     C***                eval      VACNSP = vhbNSP
024200091216     C***                eval      VACFAP = vhbFAP
024300091216     C***                eval      VACFIN = vhbFIN
024400091216     C*
024500000801     C                   ENDSR
024600091216     C*----------------------------------------------------*
024700091216     C*  IMPOSTAZIONE CAMPI COSTANTI
024800091216     C*----------------------------------------------------*
024900091216     C     DEFCAM        BEGSR
025000091216     C*
025100091216     C* ...personalizzati
025200091216     C                   eval      VACNSP = VABNSP
025300100118     C                   eval      VACLNA = 046
025400100118     C                   eval      VACHMC = 1200
025500100118     C*
025600100118     C                   if        wContaRecord = 11 + 1
025700100118     C                   eval      wContaRecord = 1
025800100118     C                   endif
025900100118     C*
026000100118     C                   select
026100100118     C* ...consegnata...
026200100118     C                   when      wContaRecord = 1
026300100118     C                   eval      VACDCM = %dec(%date() : *ISO)
026400100118     C                   eval      VACDAG = *zeros
026500100118     C                   eval      VACDLA = *zeros
026600100118     C                   eval      VACCCA = *blanks
026700100118     C* ...lasciato avviso...
026800100118     C                   when      wContaRecord = 2
026900100118     C                   eval      VACDCM = *zeros
027000100118     C                   eval      VACDAG = *zeros
027100100118     C                   eval      VACDLA = %dec(%date() : *ISO)
027200100118     C                   eval      VACCCA = *blanks
027300100118     C* ...giacenza...
027400100118     C                   when      wContaRecord = 3
027500100118     C                   eval      VACDCM = *zeros
027600100118     C                   eval      VACDAG = %dec(%date() : *ISO)
027700100118     C                   eval      VACDLA = *zeros
027800100118     C                   eval      VACCCA = *blanks
027900100118     C* ...messa in consegna...
028000100118     C                   when      wContaRecord = 4
028100100118     C                   eval      VACDCM = %dec(%date() : *ISO)
028200100118     C                   eval      VACDAG = *zeros
028300100118     C                   eval      VACDLA = *zeros
028400100118     C                   eval      VACCCA = 'C'
028500100118     C* ...dirottamento...
028600100118     C                   when      wContaRecord = 5
028700100118     C                   eval      VACDCM = *zeros
028800100118     C                   eval      VACDAG = *zeros
028900100118     C                   eval      VACDLA = *zeros
029000100118     C                   eval      VACCCA = '1'
029100100118     C* ...reso...
029200100118     C                   when      wContaRecord = 6
029300100118     C                   eval      VACDCM = %dec(%date() : *ISO)
029400100118     C                   eval      VACDAG = *zeros
029500100118     C                   eval      VACDLA = *zeros
029600100118     C                   eval      VACCCA = '2'
029700100118     C* ...apertura anomalia...
029800100118     C                   when      wContaRecord = 7
029900100118     C                   eval      VACDCM = %dec(%date() : *ISO)
030000100118     C                   eval      VACDAG = *zeros
030100100118     C                   eval      VACDLA = *zeros
030200100118     C                   eval      VACCCA = 'A'
030300100118     C* ...reso...
030400100118     C                   when      wContaRecord = 8
030500100118     C                   eval      VACDCM = %dec(%date() : *ISO)
030600100118     C                   eval      VACDAG = *zeros
030700100118     C                   eval      VACDLA = *zeros
030800100118     C                   eval      VACCCA = '2'
030900100118     C* ...anomalia conclusa negativamente...
031000100118     C                   when      wContaRecord = 9
031100100118     C                   eval      VACDCM = %dec(%date() : *ISO)
031200100118     C                   eval      VACDAG = *zeros
031300100118     C                   eval      VACDLA = *zeros
031400100118     C                   eval      VACCCA = '5'
031500100118     C* ...avaria resa...
031600100118     C                   when      wContaRecord = 10
031700100118     C                   eval      VACDCM = %dec(%date() : *ISO)
031800100118     C                   eval      VACDAG = *zeros
031900100118     C                   eval      VACDLA = *zeros
032000100118     C                   eval      VACCCA = '6'
032100100118     C* ...ripristino...
032200100118     C                   when      wContaRecord = 11
032300100118     C                   eval      VACDCM = *zeros
032400100118     C                   eval      VACDAG = *zeros
032500100118     C                   eval      VACDLA = *zeros
032600100118     C                   eval      VACCCA = 'R'
032700100118     C*
032800100118     C                   endsl
032900091216     C*
033000091216     C                   ENDSR
033100070411
033200091216
033300091216
033400091216      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
033500091216     C     wriVGD        BEGSR
033600091216     C*
033700091216     C                   clear                   tivgd000
033800091216     C                   eval      vgdDTA = fnvac00t
033900091216     C                   eval      vgdTIP = 'VC'
034000091216     C                   eval      vgdKSU = vlrKSC
034100091216     C                   eval      vgdTSC = 'WW'
034200091216     C                   eval      vgdDAT = datcor
034300091216     C                   eval      vgdPGM = 'TITV1T6R'
034400091216     C                   write     tivgd000
034500091216     C*
034600091216     C                   ENDSR
034700091216     C*------------------------------------------------------------------------*
034800091216
034900091216
035000091216
035100990910
035200000613     C     *inzsr        BEGSR
035300990910     C*
035400990910     C     *entry        plist
035500990920     C                   parm                    tivlrds
035600990921     C                   parm      wrkesito      esito
035700000724     C                   parm                    prmlit
035800000710     C                   parm                    prmfir
035900000613     C*
036000091214     C                   z-add     *zeros        datcor            8 0
036100091214     C                   eval      datcor = %dec(%date() : *ISO)
036200000830     C*
036300000613     C                   ENDSR
036400000613     C***
