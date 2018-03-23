000100061031      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500061031     FEDIVABwr  O    E             DISK    usropn
000600061031     FEDIVATwr  O    E             DISK    usropn
000700000801     D*----------------------------------------------------
000800000801     D* DICHIARAZIOINE VARIABILI DI WRK
000900000801     D*----------------------------------------------------
001000990920     D dscmz         e ds                  inz
001100990910     D psds           sds
001200990910     D  procname         *PROC
001300990920     D tivlrds       e ds                  extname(tivlr00f)
001400990910     D esito           s              1
001500000724     D prmlit          s             10
001600000710     D prmfir          s             10
001700990921     D wrkesito        s                   like(esito)
001800000613     D rrnum           s              6  0 INZ(*zeros)
001900020305     D depspe          s             14    INZ(*blanks)
002000010213     D depdat          s              8  0 INZ(*zeros)
002100010213     D depaas          s              4    INZ(*zeros)
002200020305     D curspe          s             14    INZ(*blanks)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700000801
002800010213     D                 DS                  INZ
002900010213     D  aaaa                   1      4  0
003000010213     D  mmgg                   5      8  0
003100010213     D  mm                     5      6  0
003200010213     D  gg                     7      8  0
003300010213     D  datawrk                1      8  0
003400010213
003500010213     D  Num8_0         s              8  0
003600000801     D  Num5_0         s              5  0
003700000830
003800000830     D*------------------
003900000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004000000830     D*------------------
004100000830     D WLBDA8          DS                  INZ
004200000830     D  G08DAT                 1      8  0
004300000830     D  G08INV                 9     16  0
004400000830     D  G08ERR                17     17
004500000830     D  G08TGI                18     22  0
004600000830     D*
004700990908
004800010201
004900010201
005000000913     C                   reset                   rrnum
005100990921     C                   reset                   esito
005200990921     C                   reset                   wrkesito
005300000613     C*
005400020305     C                   EXSR      RWPRADA                                      LETT/SCR. VAB
005500000613     C*
005600010202     C* Effettuo la chiamata al CLLE preposto
005700061031     C                   call(e)   'TITVEVTC'
005800010202     C                   parm                    parccm
005900010202     C                   parm                    parmbr
006000010202     C                   parm      '2'           paropz
006100000616     C*
006200000801     C
006300010201     C                   seton                                        LR
006400990908
006500000801
006600910830     C*--------------------------------------------------------
006700061031     C* RWPRADA  LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
006800910830     C*--------------------------------------------------------
006900020305     C     RWPRADA       BEGSR
007000990910     C*
007100990914     C                   if        not %open(tivin00r)
007200990908     C                   open      tivin00r
007300990914     C                   endif
007400061031     C                   if        not %open(edivabwr)
007500061031     C                   open      edivabwr
007600990914     C                   endif
007700061031     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
007800020305     C                   exsr      prevat
007900010201     C*
008000010202     C                   if        chkcall = '0'
008100010202     C*
008200061031     C                   if        not %open(edivatwr)
008300061031     C                   open      edivatwr
008400010201     C                   endif
008500990910     C*
008600010201     C                   clear                   §CTROKVB          5 0
008700020305     C                   clear                   §CTROKVT          5 0
008800000801     C                   clear                   §CTRMO            5 0
008900000801     C                   clear                   §CTRNO            5 0
009000990910     C*
009100921023     C                   DO        *HIVAL
009200990913     C*
009300990915     C                   READ      tivin00r                               70
009400000905     C                   if        vindta > *blanks
009500000613     C                   add       1             rrnum
009600000801     C*
009700000801     C                   if        *in70 = *off
009800000801     C                             and
009900000801     C                             (vinflg = *blanks
010000000801     C                              or vinflg = '0'
010100000801     C                              or vinflg = '2')
010200000801     C*
010300000801     C                   clear                   vinmsg
010400000801     C                   eval      vinflg = '1'
010500010305     C*
010600010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
010700061027     C                   EVAL      PiStr=%trim(%subst(vindta:122:14))
010800020305     C                   MOVEL(p)  PiStr         curspe
010900010305     C*
011000010305     C                   if        depspe = *blanks                             => 1° giro
011100010305     C                   eval      depspe = curspe                              => memorizz. spediz
011200020305     C                   exsr      impvab
011300090403     C  N60              exsr      wrivat_e                                     => carico VAT
011400010305     C                   else
011500010305     C                   if        curspe <> depspe                             => rottura di spediz
011600010305     C                   eval      depspe = curspe                              => memorizz. spediz
011700050202     C  N60              exsr      wrivab
011800020305     C                   exsr      impvab
011900090403     C  N60              exsr      wrivat_e                                     => carico VAT
012000020305     C                   else                                                   => x stessa spediz
012100020305     C                   exsr      impvab
012200090403     C  N60              exsr      wrivat_e                                     => carico VAT
012300010305     C                   endif
012400010305     C                   endif
012500010305     C                   endif
012600000905     C*
012700000905     C                   else
012800000905     C                   eval      vinflg = '1'
012900000905     C                   endif
013000000905     C*
013100000905     C  N70              update    tivin000
013200000905     C*
013300991022     C  N70              ENDdo
013400010202     C*
013500020305     C* Scarico i VAB rimasti "in sospeso"
013600050202     C  N60              exsr      wrivab
013700010202     C*
013800010202     C                   endif
013900990910
014000990910     C* Se non ci sono record con errori ...
014100000710     C                   if        §ctrno = 0
014200990910     C* ... restituisco esito OK.
014300990921     C                   eval      wrkesito = '0'
014400990910     C                   else
014500010201     C                   if        §ctrokvb > 0
014600990921     C                   eval      wrkesito = '1'
014700000710     C                   else
014800000710     C                   eval      wrkesito = '2'
014900990910     C                   endif
015000000710     C                   endif
015100990910     C*
015200990914     C                   if        %open(tivin00r)
015300990908     C                   close     tivin00r
015400990914     C                   endif
015500061031     C                   if        %open(edivabwr)
015600061031     C                   close     edivabwr
015700990914     C                   endif
015800061031     C                   if        %open(edivatwr)
015900061031     C                   close     edivatwr
016000010201     C                   endif
016100990910     C*
016200010201     C                   if        §ctrokvb > 0
016300000724     C                             and vlrpoi <> *zeros
016400010202     C                   exsr      invio
016500990920     C                   endif
016600990920     C*
016700910830     C                   ENDSR
016800000613     C***
016900010305
017000010305     C*----------------------------------------------------*
017100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
017200010305     C*----------------------------------------------------*
017300020305     C     WRIVAB        BEGSR
017400010305     C*
017500061031     C                   write     edivab00                                     => scarico il VAB
017600090403     C*
017700090403     C* Scarico buffer EDIVAT x estensioni testata
017800090403     C                   exsr      wrivat_a
017900090403     C                   exsr      wrivat_b
018000010305     C*
018100010305     C                   ENDSR
018200990920
018300000801     C*----------------------------------------------------*
018400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018500000801     C*----------------------------------------------------*
018600010201     C     INZVAR        BEGSR
018700000801     C*
018800010201     C                   Z-ADD     *zeros        Num5_0
018900000801     C*
019000000801     C                   ENDSR
019100000801     C*----------------------------------------------------*
019200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
019300000801     C*----------------------------------------------------*
019400000801     C     DEFCAM        BEGSR
019500000801     C*
019600061031     C                   CLEAR                   EDIVAB00
019700020305     C                   Z-ADD     0282998       VABCCM
019800020305     C                   Z-ADD     0282998       VATCCM
019900020305     C                   Z-ADD     028           VABLNP
020000020305     C                   Z-ADD     028           VATLNP
020100010201     C                   Z-ADD     000           VABCTR
020200020305     C                   MOVEL     '7Q'          VABCTM
020300000801     C*
020400000801     C                   ENDSR
020500000801     C*----------------------------------------------------*
020600061031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
020700000801     C*----------------------------------------------------*
020800010201     C     IMPVAB        BEGSR
020900000801     C*
021000020305     C                   EXSR      INZVAR
021100020305     C                   EXSR      DEFCAM
021200061031     C*
021300061031     C* VALORIZZO CAMPI RELATIVI AL "CMR"
021400061031     C                   EVAL      VABCMR = 'PRADA' + datora_A
021500061031     C                   EVAL      VABDCM = DATCOR
021600061031     C                   EVAL      VABDTS = DATCOR
021700061031     C                   EVAL      VABHMS = ORACOR
021800061031     C                   EVAL      VABCNT = 1
021900010305     C*
022000050202     C                   SETOFF                                       60
022100000801     C                   Z-ADD     *zeros        errore            1 0
022200000830     C                   MOVEL     datcor        VABAAS
022300020305     C                   MOVEL     datcor        VATAAS
022400021113     C                   MOVE(P)   vlrpoi        VABFGS
022500021113     C                   MOVE(P)   vlrpoi        VATFGS
022600010213     C                   MOVEL     VABAAS        depaas
022700000830     C                   MOVE      datcor        VABMGS
022800050511     C                   EVAL      VABRSD=%trim(%subst(vindta:225:30))
022900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
023000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
023100020117     C* ==
023200061027     C                   EVAL      VABIND=%trim(%subst(vindta:255:30))
023300061027     C                   EVAL      VABRMA=%trim(%subst(vindta:122:14))
023400061027     C                   EVAL      VABLOD=%trim(%subst(vindta:291:30))
023500061027     C                   EVAL      VABPRD=%trim(%subst(vindta:321:2))
023600061027     C                   EVAL      VABNOT=%trim(%subst(vindta:335:50))
023700061027     C                   EVAL      VABNT2=%trim(%subst(vindta:417:14))+'-'+
023800061027     C                                    %trim(%subst(vindta:385:20))
023900090403     C                   EVAL      VABTC1=%trim(%subst(vindta:603:1))
024000020305     C*
024100061027     C                   EVAL      PiStr=%trim(%subst(vindta:286:5))
024200000801     C                   EXSR      CHKNUM
024300000801     C                   IF        PiInt=*on
024400000801     C                   Z-ADD     PiVal         Num5_0
024500010208     C                   MOVEL(p)  Num5_0        VABCAD
024600000801     C                   ELSE
024700000801     C                   ADD       1             errore
024800000801     C                   EVAL      vinmsg = %trimr(vinmsg)
024900000801     C                             + ' ' + 'VABCAD'
025000000801     C                   ENDIF
025100020305     C*
025200061027     C                   EVAL      PiStr=%trim(%subst(vindta:184:6))
025300010201     C                   EXSR      CHKNUM
025400010201     C                   IF        PiInt=*on
025500010201     C                   Z-ADD     PiVal         Num5_0
025600010208     C                   MOVE      Num5_0        VABNCL
025700010201     C                   ELSE
025800010201     C                   ADD       1             errore
025900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
026000010201     C                             + ' ' + 'VABNCL'
026100010201     C                   ENDIF
026200020305     C*
026300061027     C                   EVAL      PiStr=%trim(%subst(vindta:191:10))
026400010201     C                   EXSR      CHKNUM
026500010201     C                   IF        PiNum=*on
026600011115     C                   Z-ADD     PiVal         VABPKB
026700010201     C                   ELSE
026800010201     C                   ADD       1             errore
026900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
027000010201     C                             + ' ' + 'VABPKB'
027100010201     C                   ENDIF
027200020305     C*
027300061027     C                   EVAL      PiStr=%trim(%subst(vindta:202:14))
027400010202     C                   IF        PiStr>*blanks
027500010201     C                   EXSR      CHKNUM
027600010201     C                   IF        PiNum=*on
027700020305     C                   Z-ADD(H)  PiVal         VABVLB
027800010201     C                   ELSE
027900010201     C                   ADD       1             errore
028000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
028100010201     C                             + ' ' + 'VABVLB'
028200010201     C                   ENDIF
028300010202     C                   ENDIF
028400020305     C*
028500061027     C                   EVAL      PiStr=%trim(%subst(vindta:122+7:7))
028600010201     C                   EXSR      CHKNUM
028700010201     C                   IF        PiInt=*on
028800050511     C                   Z-ADD     PiVal         VABRMN
028900011115     C                   Z-ADD     PiVal         VABNSP
029000020305     C                   Z-ADD     PiVal         VATNSP
029100010201     C                   ELSE
029200050202     C                   SETON                                        60
029300010201     C                   EVAL      vinmsg = %trimr(vinmsg)
029400050511     C                             + ' ' + 'VABNSP VATNSP VABRMN'
029500010201     C                   ENDIF
029600010205     C*
029700010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
029800010205     C                   IF        VABCAS > *zeros
029900010205     C                   EVAL      VABCBO = '4'
030000010205     C                   ELSE
030100010205     C                   EVAL      VABCBO = '1'
030200010205     C                   ENDIF
030300020305     C*
030400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
030500011113     C                   EXSR      CHKIMPDIV
030600010202     C*
030700000801     C* Ebbene...
030800050202     C                   IF        *IN60 = *ON
030900000801     C                   ADD       1             §CTRNO
031000000801     C                   EVAL      vinflg = '2'
031100000801     C                   ELSE
031200050202     C                   ADD       1             §CTROKVB
031300050202     C                   IF        errore <> *zeros
031400050202     C                   ADD       1             §CTRMO
031500050202     C                   ENDIF
031600000801     C                   ENDIF
031700000801     C*
031800000801     C                   ENDSR
031900010201     C*----------------------------------------------------*
032000061031     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
032100010201     C*----------------------------------------------------*
032200090403     C     WRIVAT_E      BEGSR
032300061031     C*
032400061031     C* VALORIZZO CAMPI RELATIVI AL "CMR"
032500061031     C                   EVAL      VATCMR = VABCMR
032600061031     C                   EVAL      VATCNT = VABCNT
032700010201     C*
032800090403     C* Valorizzo l buffer di scrittura del EDIVAT (x "CHI SONO")
032900090403     C                   MOVEL     'E'           VATTRC
033000061027     C                   eval      VATNOT = %subst(vindta:432:20)
033100061031     C                   write     EDIVAT00
033200010201     C*
033300010201     C                   ENDSR
033400090403     C*----------------------------------------------------*
033500090403     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
033600090403     C*----------------------------------------------------*
033700090403     C     WRIVAT_A      BEGSR
033800090403     C*
033900090403     C* VALORIZZO CAMPI RELATIVI AL "CMR"
034000090403     C                   EVAL      VATCMR = VABCMR
034100090403     C                   EVAL      VATCNT = VABCNT
034200090403     C*
034300090403     C* Valorizzo l buffer di scrittura del EDIVAT (x REFERENTE CONSEGNA)
034400090403     C                   MOVEL     'A'           VATTRC
034500090403     C                   eval      VATNOT = %subst(vindta:603+1:35-1)
034600090403     C                   write     EDIVAT00
034700090403     C*
034800090403     C                   ENDSR
034900090403     C*----------------------------------------------------*
035000090403     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
035100090403     C*----------------------------------------------------*
035200090403     C     WRIVAT_B      BEGSR
035300090403     C*
035400090403     C* VALORIZZO CAMPI RELATIVI AL "CMR"
035500090403     C                   EVAL      VATCMR = VABCMR
035600090403     C                   EVAL      VATCNT = VABCNT
035700090403     C*
035800090403     C* Valorizzo l buffer di scrittura del EDIVAT (x TELEFONO REFERENTE)
035900090403     C                   MOVEL     'B'           VATTRC
036000090403     C                   eval      VATNOT = %subst(vindta:498:24)
036100090403     C                   write     EDIVAT00
036200090403     C*
036300090403     C                   ENDSR
036400010202     C*----------------------------------------------------*
036500061031     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
036600010202     C*----------------------------------------------------*
036700020305     C     PREVAT        BEGSR
036800010202     C*
036900061031     C* Compongo il nome del membro da dare al EDIVATWR
037000010202     C                   eval      parmbr = vlrhdl
037100010202     C                   movel     'M'           parmbr
037200020305     C                   eval      parccm = '00282998'
037300010202     C                   eval      paropz = '1'
037400010202     C* Effettuo la chiamata al CLLE preposto
037500061031     C                   call(e)   'TITVEVTC'
037600010202     C                   parm                    parccm
037700010202     C                   parm                    parmbr
037800010202     C                   parm                    paropz
037900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
038000010202     C                   if        %error
038100010202     C                   movel     '1'           chkcall
038200010202     C                   else
038300010202     C                   movel     '0'           chkcall
038400010202     C                   endif
038500010202     C*
038600010202     C                   ENDSR
038700000801     C*----------------------------------------------------*
038800000801     C*  CONTROLLO NUMERICITA' CAMPI
038900000801     C*----------------------------------------------------*
039000000801     C     CHKNUM        BEGSR
039100000801     C*
039200000801     C                   call(e)   'ISNUMERIC'
039300000801     C                   PARM                    PiStr            30
039400000801     C                   PARM      ','           PiDecChr          1
039500000801     C                   PARM      *ZEROS        PiVal            30 9
039600000801     C                   PARM      '0'           PiInt             1
039700000801     C                   PARM      '0'           PiNum             1
039800000801     C                   IF        %error
039900000801     C                   EVAL      PiInt=*off
040000000801     C                   ENDIF
040100000801     C*
040200000801     C                   ENDSR
040300000801     C***
040400000801
040500011113
040600011113     C*----------------------------------------------------*
040700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
040800011113     C*----------------------------------------------------*
040900011113     C     CHKIMPDIV     BEGSR
041000011113     C*
041100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
041200011113     C                   Z-ADD     *zeros        wrkDec            9 9
041300011113     C*
041400011113     C* Come prima cosa effettuo considerazioni sulla divisa
041500011113     C                   IF        vabIAS > *zeros
041600011113     C                   IF        vabVAS <> 'EUR'
041700011113     C                   EVAL      vabVAS =  'ITL'
041800011113     C                   ENDIF
041900011113     C                   ENDIF
042000011113     C*
042100011113     C                   IF        vabCAS > *zeros
042200011113     C                   IF        vabVCA <> 'EUR'
042300011113     C                   EVAL      vabVCA =  'ITL'
042400011113     C                   ENDIF
042500011113     C                   ENDIF
042600011113     C*
042700011113     C                   IF        vabVMD > *zeros
042800020305     C                   IF        vabVAD <> 'EUR'
042900011113     C                   EVAL      vabVAD =  'ITL'
043000011113     C                   ENDIF
043100011113     C                   ENDIF
043200011113     C*
043300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
043400011113     C                   Z-ADD     vabIAS        wrkDec
043500011113     C                   IF        wrkDec > *zeros
043600011113     C                   IF        vabVAS = 'ITL'
043700011113     C                   EVAL      vabIAS = *zeros
043800011113     C                   ENDIF
043900011113     C                   ENDIF
044000011113     C*
044100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
044200011113     C                   Z-ADD     vabCAS        wrkDec
044300011113     C                   IF        wrkDec > *zeros
044400011113     C                   IF        vabVCA = 'ITL'
044500011113     C                   EVAL      vabCAS = *zeros
044600011113     C                   ENDIF
044700011113     C                   ENDIF
044800011113     C*
044900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
045000011113     C                   Z-ADD     vabVMD        wrkDec
045100011113     C                   IF        wrkDec > *zeros
045200011113     C                   IF        vabVAD = 'ITL'
045300011113     C                   EVAL      vabVMD = *zeros
045400011113     C                   ENDIF
045500011113     C                   ENDIF
045600011113     C*
045700011113     C                   ENDSR
045800011113     C***
045900011113
046000011113
046100000801
046200000801
046300990920      /TITLE Invio dei dati al punto operativo.
046400010202     C     invio         BEGSR
046500990920     C*
046600061031     C* 1° invio EDIVAT
046700010201     C                   reset                   dscmz
046800010201     C                   move      vlrpoi        cmzdst
046900061031     C                   eval      cmzfld = 'EDIVATWR'
047000010201     C                   eval      cmzmbd = vlrhdl
047100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
047200021009     C***                if        prmfir = *blanks
047300061031     C                   eval      cmzfla = 'EDIVAT0F'
047400061031     C                   eval      cmzmba = 'EDIVAT0F'
047500021009     C***                else
047600021009     C***                eval      cmzfla = prmfir
047700021009     C***                eval      cmzmba = prmfir
047800021009     C***                endif
047900010201     C                   eval      cmznrr = *zeros
048000020305     C                   move      §ctrokvt      cmznrr
048100021018     C                   eval      cmzlba = vlrfl1
048200010201     C                   call(e)   'TIS711C'
048300010201     C                   parm                    dscmz
048400010201     C                   parm      *blanks       esito
048500010205     C                   if        %error
048600010205     C                             or cmzerr = '1'
048700010205     C                             or esito  = '1'
048800010205     C                   eval      wrkesito = '3'
048900010205     C                   else
049000010201     C*
049100061031     C* 2° invio EDIVAB
049200010201     C                   reset                   dscmz
049300010201     C                   move      vlrpoi        cmzdst
049400010201     C                   eval      cmzfld = vlrfou
049500010201     C                   eval      cmzmbd = vlrhdl
049600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
049700021009     C***                if        prmfir = *blanks
049800061031     C                   eval      cmzfla = 'EDIVAB0F'
049900061031     C                   eval      cmzmba = 'EDIVAB0F'
050000021009     C***                else
050100021009     C***                eval      cmzfla = prmfir
050200021009     C***                eval      cmzmba = prmfir
050300021009     C***                endif
050400010201     C                   eval      cmznrr = *zeros
050500010201     C                   move      §ctrokvb      cmznrr
050600021018     C                   eval      cmzlba = vlrfl1
050700010201     C                   call(e)   'TIS711C'
050800010201     C                   parm                    dscmz
050900010201     C                   parm      *blanks       esito
051000010201     C                   if        %error
051100010201     C                             or cmzerr = '1'
051200010201     C                             or esito  = '1'
051300010201     C                   eval      wrkesito = '3'
051400010201     C                   endif
051500010205     C                   endif
051600990920     C*
051700000613     C                   ENDSR
051800000613     C***
051900070411
052000070411     C     *pssr         BEGSR
052100070411     C*
052200070411     C                   if        %open(tivin00r)
052300070411     C                   close     tivin00r
052400070411     C                   endif
052500070502     C                   if        %open(edivabwr)
052600070502     C                   close     edivabwr
052700070411     C                   endif
052800070502     C                   if        %open(edivatwr)
052900070502     C                   close     edivatwr
053000070411     C                   endif
053100070411     C*
053200070411     C* Effettuo la chiamata al CLLE preposto
053300070502     C                   call(e)   'TITVEVTC'
053400070502     C                   parm                    parccm
053500070502     C                   parm                    parmbr
053600070502     C                   parm      '2'           paropz
053700070411     C*
053800070411     C                   eval      wrkesito = '2'
053900070411     C*
054000070411     C                   seton                                        LR
054100070411     C*
054200070411     C                   ENDSR     '*CANCL'
054300070411     C***
054400070411
054500990910
054600000613     C     *inzsr        BEGSR
054700990910     C*
054800990910     C     *entry        plist
054900990920     C                   parm                    tivlrds
055000990921     C                   parm      wrkesito      esito
055100000724     C                   parm                    prmlit
055200000710     C                   parm                    prmfir
055300000613     C*
055400000830     C* CALCOLA LA DATA CORRENTE
055500000830     C                   time                    wn14             14 0
055600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
055700061031     C                   movel     wn14          oracor            6 0          *ORA
055800000830     C                   z-add     wn8           g08dat
055900000830     C                   z-add     *zeros        g08inv
056000000830     C                   movel     '0'           g08err
056100000830     C                   call      'XSRDA8'
056200000830     C                   parm                    wlbda8
056300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
056400061031     C*
056500061031     C* Compongo data/ora in formato alfa
056600061031     C                   movel(P)  datcor        datora_A         14
056700061031     C                   move      oracor        datora_A
056800000830     C*
056900000613     C                   ENDSR
057000000613     C***
