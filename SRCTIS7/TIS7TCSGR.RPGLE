000100090114      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130220     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
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
001400130219     D tnvas00t      e ds
001500130219     D fnvag00t      e ds
001600130219     D fnvabaar      e ds
001700990910     D esito           s              1
001800091216     D errore          s              6  0 inz
001900000724     D prmlit          s             10
002000000710     D prmfir          s             10
002100990921     D wrkesito        s                   like(esito)
002200990915     D wrkdata         s               d
002300990915     D wrkora          s               t
002400100118     D wContaRecord    s              2  0 inz
002500091216
002600130220     D*---------------------------------------------------------
002700130220     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
002800130220     D*---------------------------------------------------------
002900130220     D* 10+3
003000130220     D                 DS
003100130220     D  WS15                   1      1
003200130220     D  WI15                   2     11
003300130220     D  WV15                  12     12
003400130220     D  WD15                  13     15
003500130220     D  WASC15                 1     15
003600130220     D*  6+1
003700130220     D                 DS
003800130220     D  WS09                   1      1
003900130220     D  WI09                   2      7
004000130220     D  WV09                   8      8
004100130220     D  WD09                   9      9
004200130220     D  WASC09                 1      9
004300130220     D*  2+3
004400130220     D                 DS
004500130220     D  WS07                   1      1
004600130220     D  WI07                   2      3
004700130220     D  WV07                   4      4
004800130220     D  WD07                   5      7
004900130220     D  WASC07                 1      7
005000130220     D*---------------------------------------------------------
005100091216
005200091216     D*---------------------------------------------------------
005300091216     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
005400091216     D*---------------------------------------------------------
005500130220     D*                ds
005600130220     D*vhbatb                         1
005700130220     D*vhbccm                         7
005800130220     D*vhblnp                         3
005900130220     D*vhbaas                         4
006000130220     D*vhbmgs                         4
006100130220     D*vhbnrs                         2
006200130220     D*vhbnsp                         7
006300130220     D*vhbcbo                         2
006400130220     D*vhblna                         3
006500130220     D*vhbrsd                        35
006600130220     D*vhbrd2                        35
006700130220     D*vhbind                        35
006800130220     D*vhbcad                         9
006900130220     D*vhblod                        35
007000130220     D*vhbprd                         2
007100130220     D*vhbnzd                         3
007200130220     D*vhbgc1                         2
007300130220     D*vhbgc2                         2
007400130220     D*vhbctr                         3
007500130220     D*vhbtsp                         1
007600130220     D*vhbias                        13
007700130220     D*vhbvas                         3
007800130220     D*vhbnas                        15
007900130220     D*vhbncl                         5
008000130220     D*vhbpkb                         7
008100130220     D*vhbvlb                         5
008200130220     D*vhbqft                        13
008300130220     D*vhbcas                        13
008400130220     D*vhbtic                         2
008500130220     D*vhbvca                         3
008600130220     D*vhbgca                         2
008700130220     D*vhbrmn                        15
008800130220     D*vhbrma                        15
008900130220     D*vhbncd                         7
009000130220     D*vhbnca                         7
009100130220     D*vhbxco                         1
009200130220     D*vhbnot                        35
009300130220     D*vhbnt2                        35
009400130220     D*vhbznc                         2
009500130220     D*vhbctm                         2
009600130220     D*vhbffd                         1
009700130220     D*vhbdcr                         8
009800130220     D*vhbtcr                         1
009900130220     D*vhbhcr                         4
010000130220     D*vhbcts                         2
010100130220     D*vhbftm                         1
010200130220     D*vhbvmd                        13
010300130220     D*vhbvad                         3
010400130220     D*vhbgma                         2
010500130220     D*vhbgga                         2
010600130220     D*vhbgva                         2
010700130220     D*vhbtc1                         1
010800130220     D*vhbtc2                         1
010900130220     D*vhbscl                         1
011000130220     D*vhbant                         9
011100130220     D*vhbrmo                        25
011200130220     D*vhbcmo                         9
011300130220     D*vhbnmo                         3
011400130220     D* X_rec                  1    486    inz
011500091216     D*
011600090114
011700091215     D*------------------
011800091215     D* DS REPERIMENTO NUMERATORE
011900091215     D*------------------
012000091215     D trul33ds      e ds                  inz
012100091215     D*------------------
012200091215     D* DS ARCHITETTURA
012300091215     D*------------------
012400091215     D kpjba         e ds                  inz
012500130220
012600130220
012700130220     D*------------------
012800130220     D* LINKING A DEFINIZIONI ESTERNE
012900130220     D*------------------
013000130220     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
013100130220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
013200081217
013300010201
013400081217
013500081217     C
013600990921     C                   reset                   wrkesito
013700000613     C*
013800040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
013900000616     C*
014000000801     C
014100010201     C                   seton                                        LR
014200990908
014300000801
014400910830     C*--------------------------------------------------------
014500090114     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
014600910830     C*--------------------------------------------------------
014700040526     C     RWFILE        BEGSR
014800990910     C*
014900990914     C                   if        not %open(tivin00r)
015000990908     C                   open      tivin00r
015100990914     C                   endif
015200990910     C*
015300100118     C                   eval      wContaRecord = 0
015400100118     C*
015500921023     C                   DO        *HIVAL
015600990913     C*
015700990915     C                   READ      tivin00r                               70
015800050627     C                   if        vindta > *blanks
015900000801     C*
016000091214     C                   if        *in70 = *off
016100000801     C                             and
016200000801     C                             (vinflg = *blanks
016300000801     C                              or vinflg = '0'
016400000801     C                              or vinflg = '2')
016500100118     C*
016600100118     C                   eval      wContaRecord = wContaRecord + 1
016700000801     C*
016800000801     C                   clear                   vinmsg
016900000801     C                   eval      vinflg = '1'
017000091216     C                   exsr      inzvar
017100091216     C                   exsr      impvab
017200091216     C                   exsr      repnsp
017300130220     C   41              exsr      defcam_VC
017400130220     C   42              exsr      defcam_VS
017500130220     C   43              exsr      defcam_VG
017600130219     C   41              exsr      wrivgd_VC
017700130219     C   42              exsr      wrivgd_VS
017800130219     C   43              exsr      wrivgd_VG
017900000905     C*
018000000905     C                   else
018100000905     C                   eval      vinflg = '1'
018200050628     C                   endif
018300000905     C                   endif
018400000905     C*
018500000905     C  N70              update    tivin000
018600000905     C*
018700991022     C  N70              ENDdo
018800010202     C*
018900990910
019000091216     C*
019100091216     C                   if        errore > *zeros
019200091216     C                   eval      wrkesito = '6'
019300091216     C                   else
019400091216     C                   eval      wrkesito = '0'
019500091216     C                   endif
019600091216     C*
019700990914     C                   if        %open(tivin00r)
019800990908     C                   close     tivin00r
019900990914     C                   endif
020000990920     C*
020100910830     C                   ENDSR
020200000613     C***
020300990920
020400000801     C*----------------------------------------------------*
020500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020600000801     C*----------------------------------------------------*
020700010201     C     INZVAR        BEGSR
020800000801     C*
020900130219     C                   CLEAR                   FNVAC00T
021000130219     C                   CLEAR                   TNVAS00T
021100130219     C                   CLEAR                   FNVAG00T
021200130219     C*
021300130219     C* Verifico i parametri di input
021400130219     C                   setoff                                       414243
021500130219     C*
021600130220     C                   if        %subst(vlrppt:1:1) = 'S'                     * Crea VC
021700130219     C                   seton                                        41
021800130219     C                   endif
021900130220     C                   if        %subst(vlrppt:2:1) = 'S'                     * Crea VS
022000130219     C                   seton                                        42
022100130219     C                   endif
022200130220     C                   if        %subst(vlrppt:3:1) = 'S'                     * Crea VG
022300130219     C                   seton                                        43
022400130219     C                   endif
022500000801     C*
022600000801     C                   ENDSR
022700091215     C*----------------------------------------------------*
022800091215     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
022900091215     C*----------------------------------------------------*
023000091215     C     REPNSP        BEGSR
023100091215     C*
023200091215     C* NSP => Stacco un numeratore da AZNUM
023300091215     C                   clear                   TRUL33DS
023400091215     C                   eval      I33OPE = *zeros
023500091215     C                   eval      I33CNU = 302
023600091215     C                   eval      I33NUM = 1
023700091215     C                   movel     TRUL33DS      KPJBU
023800091215     C                   call      'TRUL33R'
023900091215     C                   parm                    KPJBA
024000091215     C                   movel     KPJBU         TRUL33DS
024100091215     C                   if        O33ERR = *zeros
024200091216     C                   z-add     O33NRF        VABNSP            7 0
024300091215     C                   else
024400091215     C                   ADD       1             errore
024500091215     C                   EVAL      vinmsg = %trimr(vinmsg)
024600091216     C                             + ' ' + 'VABNSP'
024700091215     C                   endif
024800091216     C*
024900091215     C                   ENDSR
025000000801     C*----------------------------------------------------*
025100090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
025200000801     C*----------------------------------------------------*
025300040823     C     IMPVAB        BEGSR
025400091216     C*
025500130220     C*                  eval      x_rec    = vindta
025600130220     C                   eval      fnvabaar = vindta
025700091216     C*
025800130219     C* Richiesta generazione VC
025900130219     C                   if        *in41
026000091216     C* ...numerici
026100130219     C                   move(p)   vhbAAS        VACAAS
026200091216     C                   move(p)   vhbLNP        VACLNP
026300091216     C                   move(p)   vhbNRS        VACNRS
026400091216     C                   move(p)   vhbMGS        VACMGS
026500091216     C                   move(p)   vhbCTR        VACCTR
026600130220     C*
026700130220     C                   EVAL      PiStr=vhbIAS
026800130220     C                   EXSR      CHKNUM
026900130220     C                   IF        PiNum=*on
027000130220     C                   Z-ADD     PiVal         VACIAS
027100130220     C                   ENDIF
027200130220     C*
027300091216     C                   move(p)   vhbNCL        VACNCL
027400130220     C*
027500130220     C                   EVAL      PiStr=vhbPKB
027600130220     C                   EXSR      CHKNUM
027700130220     C                   IF        PiNum=*on
027800130220     C                   Z-ADD     PiVal         VACPKB
027900130220     C                   ENDIF
028000130220     C*
028100130220     C                   EVAL      PiStr=vhbVLB
028200130220     C                   EXSR      CHKNUM
028300130220     C                   IF        PiNum=*on
028400130220     C                   Z-ADD     PiVal         VACVLB
028500130220     C                   ENDIF
028600130220     C*
028700130220     C                   EVAL      PiStr=vhbQFT
028800130220     C                   EXSR      CHKNUM
028900130220     C                   IF        PiNum=*on
029000130220     C                   Z-ADD     PiVal         VACQFT
029100130220     C                   ENDIF
029200130220     C*
029300130220     C                   EVAL      PiStr=vhbCAS
029400130220     C                   EXSR      CHKNUM
029500130220     C                   IF        PiNum=*on
029600130220     C                   Z-ADD     PiVal         VACCAS
029700130220     C                   ENDIF
029800130220     C*
029900091216     C                   move(p)   vhbRMN        VACRMN
030000091216     C                   move(p)   vhbDCR        VACDCR
030100091216     C                   move(p)   vhbHCR        VACHCR
030200091216     C                   move(p)   vhbCCM        VACCCM
030300091216     C* ...alfabetici
030400091216     C                   eval      VACCBO = vhbCBO
030500091216     C                   eval      VACRSD = vhbRSD
030600091216     C                   eval      VACPRD = vhbPRD
030700091216     C                   eval      VACGC1 = vhbGC1
030800091216     C                   eval      VACGC2 = vhbGC2
030900091216     C                   eval      VACCTS = vhbCTS
031000091216     C                   eval      VACFTM = vhbFTM
031100091216     C                   eval      VACTSP = vhbTSP
031200091216     C                   eval      VACVAS = vhbVAS
031300091216     C                   eval      VACNAS = vhbNAS
031400091216     C                   eval      VACTIC = vhbTIC
031500091216     C                   eval      VACVCA = vhbVCA
031600091216     C                   eval      VACRMA = vhbRMA
031700091216     C                   eval      VACRMO = vhbRMO
031800091216     C                   eval      VACFFD = vhbFFD
031900091216     C                   eval      VACTCR = vhbTCR
032000091216     C                   eval      VACTC1 = vhbTC1
032100091216     C                   eval      VACTC2 = vhbTC2
032200130219     C                   endif
032300091216     C*
032400130219     C* Richiesta generazione VS
032500130219     C                   if        *in42
032600130219     C* ...numerici
032700130219     C                   move(p)   vhbLNP        VASLNP
032800130219     C                   move(p)   vhbAAS        VASAAS
032900130219     C                   move(p)   vhbNRS        VASNRS
033000130219     C                   move(p)   vhbRMN        VASRMN
033100130220     C*
033200130220     C                   EVAL      PiStr=vhbCAS
033300130220     C                   EXSR      CHKNUM
033400130220     C                   IF        PiNum=*on
033500130220     C                   Z-ADD     PiVal         VASCAS
033600130220     C                   ENDIF
033700130220     C*
033800130219     C* ...alfabetici
033900130219     C                   move(p)   vhbVCA        VASVCA
034000130219     C                   endif
034100130219     C*
034200130219     C* Richiesta generazione VG
034300130219     C                   if        *in43
034400130219     C* ...numerici
034500130219     C                   move(p)   vhbLNP        VAGLNP
034600130219     C                   move(p)   vhbAAS        VAGAAS
034700130219     C                   move(p)   vhbNRS        VAGNRS
034800130219     C                   move(p)   vhbRMN        VAGRMN
034900130219     C                   move(p)   vhbCCM        VAGSCM
035000130219     C* ...alfabetici
035100130219     C                   eval      VAGRMA = vhbRMA
035200130219     C                   endif
035300130219     C*
035400000801     C                   ENDSR
035500091216     C*----------------------------------------------------*
035600091216     C*  IMPOSTAZIONE CAMPI COSTANTI
035700091216     C*----------------------------------------------------*
035800130219     C     DEFCAM_VC     BEGSR
035900091216     C*
036000091216     C* ...personalizzati
036100130219     C                   eval      VACNSP = VABNSP
036200100118     C                   eval      VACLNA = 046
036300100118     C                   eval      VACHMC = 1200
036400100118     C*
036500100118     C                   if        wContaRecord = 11 + 1
036600100118     C                   eval      wContaRecord = 1
036700100118     C                   endif
036800100118     C*
036900100118     C                   select
037000100118     C* ...consegnata...
037100100118     C                   when      wContaRecord = 1
037200100118     C                   eval      VACDCM = %dec(%date() : *ISO)
037300100118     C                   eval      VACDAG = *zeros
037400100118     C                   eval      VACDLA = *zeros
037500100118     C                   eval      VACCCA = *blanks
037600100118     C* ...lasciato avviso...
037700100118     C                   when      wContaRecord = 2
037800100118     C                   eval      VACDCM = *zeros
037900100118     C                   eval      VACDAG = *zeros
038000100118     C                   eval      VACDLA = %dec(%date() : *ISO)
038100100118     C                   eval      VACCCA = *blanks
038200100118     C* ...giacenza...
038300100118     C                   when      wContaRecord = 3
038400100118     C                   eval      VACDCM = *zeros
038500100118     C                   eval      VACDAG = %dec(%date() : *ISO)
038600100118     C                   eval      VACDLA = *zeros
038700100118     C                   eval      VACCCA = *blanks
038800100118     C* ...messa in consegna...
038900100118     C                   when      wContaRecord = 4
039000100118     C                   eval      VACDCM = %dec(%date() : *ISO)
039100100118     C                   eval      VACDAG = *zeros
039200100118     C                   eval      VACDLA = *zeros
039300100118     C                   eval      VACCCA = 'C'
039400100118     C* ...dirottamento...
039500100118     C                   when      wContaRecord = 5
039600100118     C                   eval      VACDCM = *zeros
039700100118     C                   eval      VACDAG = *zeros
039800100118     C                   eval      VACDLA = *zeros
039900100118     C                   eval      VACCCA = '1'
040000100118     C* ...reso...
040100100118     C                   when      wContaRecord = 6
040200100118     C                   eval      VACDCM = %dec(%date() : *ISO)
040300100118     C                   eval      VACDAG = *zeros
040400100118     C                   eval      VACDLA = *zeros
040500100118     C                   eval      VACCCA = '2'
040600100118     C* ...apertura anomalia...
040700100118     C                   when      wContaRecord = 7
040800100118     C                   eval      VACDCM = %dec(%date() : *ISO)
040900100118     C                   eval      VACDAG = *zeros
041000100118     C                   eval      VACDLA = *zeros
041100100118     C                   eval      VACCCA = 'A'
041200100118     C* ...reso...
041300100118     C                   when      wContaRecord = 8
041400100118     C                   eval      VACDCM = %dec(%date() : *ISO)
041500100118     C                   eval      VACDAG = *zeros
041600100118     C                   eval      VACDLA = *zeros
041700100118     C                   eval      VACCCA = '2'
041800100118     C* ...anomalia conclusa negativamente...
041900100118     C                   when      wContaRecord = 9
042000100118     C                   eval      VACDCM = %dec(%date() : *ISO)
042100100118     C                   eval      VACDAG = *zeros
042200100118     C                   eval      VACDLA = *zeros
042300100118     C                   eval      VACCCA = '5'
042400100118     C* ...avaria resa...
042500100118     C                   when      wContaRecord = 10
042600100118     C                   eval      VACDCM = %dec(%date() : *ISO)
042700100118     C                   eval      VACDAG = *zeros
042800100118     C                   eval      VACDLA = *zeros
042900100118     C                   eval      VACCCA = '6'
043000100118     C* ...ripristino...
043100100118     C                   when      wContaRecord = 11
043200100118     C                   eval      VACDCM = *zeros
043300100118     C                   eval      VACDAG = *zeros
043400100118     C                   eval      VACDLA = *zeros
043500100118     C                   eval      VACCCA = 'R'
043600100118     C*
043700100118     C                   endsl
043800091216     C*
043900091216     C                   ENDSR
044000130219
044100130219
044200130219     C*----------------------------------------------------*
044300130219     C*  IMPOSTAZIONE CAMPI COSTANTI
044400130219     C*----------------------------------------------------*
044500130219     C     DEFCAM_VS     BEGSR
044600130219     C*
044700130219     C* ...personalizzati
044800130219     C                   eval      VASNSP = VABNSP
044900130219     C                   eval      VASLNA = 046
045000130219     C*
045100130219     C                   eval      VASNDP = 0102034
045200130220     C                   eval      VASDDP = %dec(%date()+%days(1):*ISO)
045300130219     C                   eval      VASCCB = '123456789012'
045400130219     C                   eval      VASABP = 02008
045500130219     C                   eval      VASCAP = 016919
045600130219     C                   eval      VASFPC = '2'
045700130220     C                   eval      VASDVA = %dec(%date()+%days(4):*ISO)
045800130219     C*
045900130219     C                   ENDSR
046000130219
046100130219
046200130219     C*----------------------------------------------------*
046300130219     C*  IMPOSTAZIONE CAMPI COSTANTI
046400130219     C*----------------------------------------------------*
046500130219 xxx C     DEFCAM_VG     BEGSR
046600130219     C*
046700130219     C* ...personalizzati
046800130219     C                   eval      VAGNSP = VABNSP
046900130219     C                   eval      VAGLNA = 046
047000130219     C*
047100130220     C                   eval      VAGAGC = *zeros
047200130220     C                   eval      VAGMGC = *zeros
047300130220     C                   eval      VAGFGC = *zeros
047400130220     C                   eval      VAGNGC = *zeros
047500130220     C                   eval      VAGFRG = *zeros
047600130220     C                   eval      VAGGGA = *zeros
047700130220     C                   eval      VAGCMC = *zeros
047800130220     C                   eval      VAGDMC = *zeros
047900130220     C                   eval      VAGNOT = *zeros
048000130220     C                   eval      VAGDMM = *zeros
048100130220     C                   eval      VAGVCS = *zeros
048200130220     C                   eval      VAGCAS = *zeros
048300130220     C                   eval      VAGVCA = *zeros
048400130220     C                   eval      VAGDDM = *zeros
048500130220     C                   eval      VAGDED = *zeros
048600130220     C                   eval      VAGDUR = *zeros
048700130220     C                   eval      VAGDCG = *zeros
048800130220     C                   eval      VAGCFG = *zeros
048900130220     C                   eval      VAGSGS = *zeros
049000130220     C                   eval      VAGSGR = *zeros
049100130220     C                   eval      VAGSGD = *zeros
049200130220     C                   eval      VAGDSD = *zeros
049300130220     C                   eval      VAGDSA = *zeros
049400130220     C                   eval      VAGGGS = *zeros
049500130220     C                   eval      VAGSGF = *zeros
049600130220     C                   eval      VAGSGA = *zeros
049700130220     C                   eval      VAGTFT = *zeros
049800130220     C                   eval      VAGDBG = *zeros
049900130220     C                   eval      VAGLPW = *zeros
050000130220     C                   eval      VAGASW = *zeros
050100130220     C                   eval      VAGNSW = *zeros
050200130220     C                   eval      VAGNBW = *zeros
050300130220     C                   eval      VAGKSC = *zeros
050400130220     C*
050500130220     C                   ENDSR
050600130220
050700130219
050800091216
050900130219      /TITLE Scrittura record di output in file TIVGD00F (file VAS generico download)
051000130219     C     wriVGD_VC     BEGSR
051100091216     C*
051200091216     C                   clear                   tivgd000
051300091216     C                   eval      vgdDTA = fnvac00t
051400091216     C                   eval      vgdTIP = 'VC'
051500091216     C                   eval      vgdKSU = vlrKSC
051600091216     C                   eval      vgdTSC = 'WW'
051700091216     C                   eval      vgdDAT = datcor
051800130219     C                   eval      vgdPGM = 'TIS7TCSGR'
051900091216     C                   write     tivgd000
052000091216     C*
052100091216     C                   ENDSR
052200091216     C*------------------------------------------------------------------------*
052300130219
052400130219
052500130219      /TITLE Scrittura record di output in file TIVGD00F (file VAS generico download)
052600130219     C     wriVGD_VS     BEGSR
052700130219     C*
052800130219     C                   clear                   tivgd000
052900130219     C                   eval      vgdDTA = tnvas00t
053000130219     C                   eval      vgdTIP = 'VS'
053100130219     C                   eval      vgdKSU = vlrKSC
053200130219     C                   eval      vgdTSC = 'WW'
053300130219     C                   eval      vgdDAT = datcor
053400130219     C                   eval      vgdPGM = 'TIS7TCSGR'
053500130219     C                   write     tivgd000
053600130219     C*
053700130219     C                   ENDSR
053800130219     C*------------------------------------------------------------------------*
053900130219
054000130219
054100130219      /TITLE Scrittura record di output in file TIVGD00F (file VAS generico download)
054200130219     C     wriVGD_VG     BEGSR
054300130219     C*
054400130219     C                   clear                   tivgd000
054500130219     C                   eval      vgdDTA = fnvag00t
054600130219     C                   eval      vgdTIP = 'VG'
054700130219     C                   eval      vgdKSU = vlrKSC
054800130219     C                   eval      vgdTSC = 'WW'
054900130219     C                   eval      vgdDAT = datcor
055000130219     C                   eval      vgdPGM = 'TIS7TCSGR'
055100130219     C                   write     tivgd000
055200130219     C*
055300130219     C                   ENDSR
055400130219     C*------------------------------------------------------------------------*
055500091216
055600091216
055700130220     C*----------------------------------------------------*
055800130220     C*  CONTROLLO NUMERICITA' CAMPI
055900130220     C*----------------------------------------------------*
056000130220     C     CHKNUM        BEGSR
056100130220     C*
056200130220     C                   IF        PiDecChr = *blanks
056300130220     C                   EVAL      PiDecChr = '.'
056400130220     C                   ENDIF
056500130220     C*
056600130220     C                   callp(e)  UBISNUM_Check(PiStr
056700130220     C                                          :PiDecChr
056800130220     C                                          :PiVal
056900130220     C                                          :PiNum
057000130220     C                                          :PiInt)
057100130220     C*
057200130220     C                   IF        %error
057300130220     C                   EVAL      PiInt=*off
057400130220     C                   ENDIF
057500130220     C*
057600130220     C                   ENDSR
057700130220     C***
057800091216
057900990910
058000000613     C     *inzsr        BEGSR
058100990910     C*
058200990910     C     *entry        plist
058300990920     C                   parm                    tivlrds
058400990921     C                   parm      wrkesito      esito
058500000724     C                   parm                    prmlit
058600000710     C                   parm                    prmfir
058700000613     C*
058800091214     C                   z-add     *zeros        datcor            8 0
058900091214     C                   eval      datcor = %dec(%date() : *ISO)
059000000830     C*
059100000613     C                   ENDSR
059200000613     C***
