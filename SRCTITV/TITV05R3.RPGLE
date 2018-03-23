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
001400150611     D*------------------
001500150611     D* DS REPERIMENTO NUMERATORE
001600150611     D*------------------
001700150611     D trul33ds      e ds                  inz
001800150611     D*------------------
001900150611     D* DS ARCHITETTURA
002000150611     D*------------------
002100150611     D kpjba         e ds                  inz
002200150611     D*------------------
002300990910     D esito           s              1
002400000724     D prmlit          s             10
002500000710     D prmfir          s             10
002600990921     D wrkesito        s                   like(esito)
002700000613     D rrnum           s              6  0 INZ(*zeros)
002800020305     D depspe          s             14    INZ(*blanks)
002900010213     D depdat          s              8  0 INZ(*zeros)
003000010213     D depaas          s              4    INZ(*zeros)
003100020305     D curspe          s             14    INZ(*blanks)
003200010202     D parccm          s              8    INZ(*blanks)
003300010202     D parmbr          s             10    INZ(*blanks)
003400010202     D paropz          s              1    INZ(*blanks)
003500010202     D chkcall         s              1    INZ(*blanks)
003600000801
003700010213     D                 DS                  INZ
003800010213     D  aaaa                   1      4  0
003900010213     D  mmgg                   5      8  0
004000010213     D  mm                     5      6  0
004100010213     D  gg                     7      8  0
004200010213     D  datawrk                1      8  0
004300010213
004400010213     D  Num8_0         s              8  0
004500000801     D  Num5_0         s              5  0
004600150610     D  w70            s             70
004700000830
004800000830     D*------------------
004900000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
005000000830     D*------------------
005100000830     D WLBDA8          DS                  INZ
005200000830     D  G08DAT                 1      8  0
005300000830     D  G08INV                 9     16  0
005400000830     D  G08ERR                17     17
005500000830     D  G08TGI                18     22  0
005600000830     D*
005700990908
005800010201
005900010201
006000000913     C                   reset                   rrnum
006100990921     C                   reset                   esito
006200990921     C                   reset                   wrkesito
006300000613     C*
006400020305     C                   EXSR      RWPRADA                                      LETT/SCR. VAB
006500000613     C*
006600010202     C* Effettuo la chiamata al CLLE preposto
006700061031     C                   call(e)   'TITVEVTC'
006800010202     C                   parm                    parccm
006900010202     C                   parm                    parmbr
007000010202     C                   parm      '2'           paropz
007100000616     C*
007200000801     C
007300010201     C                   seton                                        LR
007400990908
007500000801
007600910830     C*--------------------------------------------------------
007700061031     C* RWPRADA  LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
007800910830     C*--------------------------------------------------------
007900020305     C     RWPRADA       BEGSR
008000990910     C*
008100990914     C                   if        not %open(tivin00r)
008200990908     C                   open      tivin00r
008300990914     C                   endif
008400061031     C                   if        not %open(edivabwr)
008500061031     C                   open      edivabwr
008600990914     C                   endif
008700061031     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
008800020305     C                   exsr      prevat
008900010201     C*
009000010202     C                   if        chkcall = '0'
009100010202     C*
009200061031     C                   if        not %open(edivatwr)
009300061031     C                   open      edivatwr
009400010201     C                   endif
009500990910     C*
009600010201     C                   clear                   §CTROKVB          5 0
009700020305     C                   clear                   §CTROKVT          5 0
009800000801     C                   clear                   §CTRMO            5 0
009900000801     C                   clear                   §CTRNO            5 0
010000990910     C*
010100921023     C                   DO        *HIVAL
010200990913     C*
010300990915     C                   READ      tivin00r                               70
010400000905     C                   if        vindta > *blanks
010500000613     C                   add       1             rrnum
010600000801     C*
010700000801     C                   if        *in70 = *off
010800000801     C                             and
010900000801     C                             (vinflg = *blanks
011000000801     C                              or vinflg = '0'
011100000801     C                              or vinflg = '2')
011200000801     C*
011300000801     C                   clear                   vinmsg
011400000801     C                   eval      vinflg = '1'
011500010305     C*
011600010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
011700150611     C                   EVAL      PiStr=%trim(%subst(vindta:27:12))
011800020305     C                   MOVEL(p)  PiStr         curspe
011900010305     C*
012000010305     C                   if        depspe = *blanks                             => 1° giro
012100010305     C                   eval      depspe = curspe                              => memorizz. spediz
012200150611     C* Stacco un progressivo x numero spedizione
012300150611 xxx C                   EXSR      repNSP
012400020305     C                   exsr      impvab
012500090403     C  N60              exsr      wrivat_e                                     => carico VAT
012600010305     C                   else
012700010305     C                   if        curspe <> depspe                             => rottura di spediz
012800010305     C                   eval      depspe = curspe                              => memorizz. spediz
012900050202     C  N60              exsr      wrivab
013000150611     C* Stacco un progressivo x numero spedizione
013100150611 xxx C                   EXSR      repNSP
013200020305     C                   exsr      impvab
013300090403     C  N60              exsr      wrivat_e                                     => carico VAT
013400020305     C                   else                                                   => x stessa spediz
013500020305     C                   exsr      impvab
013600090403     C  N60              exsr      wrivat_e                                     => carico VAT
013700010305     C                   endif
013800010305     C                   endif
013900010305     C                   endif
014000000905     C*
014100000905     C                   else
014200000905     C                   eval      vinflg = '1'
014300000905     C                   endif
014400000905     C*
014500000905     C  N70              update    tivin000
014600000905     C*
014700991022     C  N70              ENDdo
014800010202     C*
014900020305     C* Scarico i VAB rimasti "in sospeso"
015000050202     C  N60              exsr      wrivab
015100010202     C*
015200010202     C                   endif
015300990910
015400990910     C* Se non ci sono record con errori ...
015500000710     C                   if        §ctrno = 0
015600990910     C* ... restituisco esito OK.
015700990921     C                   eval      wrkesito = '0'
015800990910     C                   else
015900010201     C                   if        §ctrokvb > 0
016000990921     C                   eval      wrkesito = '1'
016100000710     C                   else
016200000710     C                   eval      wrkesito = '2'
016300990910     C                   endif
016400000710     C                   endif
016500990910     C*
016600990914     C                   if        %open(tivin00r)
016700990908     C                   close     tivin00r
016800990914     C                   endif
016900061031     C                   if        %open(edivabwr)
017000061031     C                   close     edivabwr
017100990914     C                   endif
017200061031     C                   if        %open(edivatwr)
017300061031     C                   close     edivatwr
017400010201     C                   endif
017500990910     C*
017600010201     C                   if        §ctrokvb > 0
017700000724     C                             and vlrpoi <> *zeros
017800010202     C                   exsr      invio
017900990920     C                   endif
018000990920     C*
018100910830     C                   ENDSR
018200000613     C***
018300010305
018400010305     C*----------------------------------------------------*
018500020305     C*  SCARICAMENTO BUFFER RECORDS VAB
018600010305     C*----------------------------------------------------*
018700020305     C     WRIVAB        BEGSR
018800010305     C*
018900061031     C                   write     edivab00                                     => scarico il VAB
019000090403     C*
019100090403     C* Scarico buffer EDIVAT x estensioni testata
019200150610     C***                exsr      wrivat_a
019300150610     C***                exsr      wrivat_b
019400010305     C*
019500010305     C                   ENDSR
019600990920
019700000801     C*----------------------------------------------------*
019800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019900000801     C*----------------------------------------------------*
020000010201     C     INZVAR        BEGSR
020100000801     C*
020200010201     C                   Z-ADD     *zeros        Num5_0
020300000801     C*
020400000801     C                   ENDSR
020500000801     C*----------------------------------------------------*
020600000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020700000801     C*----------------------------------------------------*
020800000801     C     DEFCAM        BEGSR
020900000801     C*
021000061031     C                   CLEAR                   EDIVAB00
021100140205     C                   Z-ADD     0288598       VABCCM
021200140205     C                   Z-ADD     0288598       VATCCM
021300020305     C                   Z-ADD     028           VABLNP
021400020305     C                   Z-ADD     028           VATLNP
021500010201     C                   Z-ADD     000           VABCTR
021600020305     C                   MOVEL     '7Q'          VABCTM
021700000801     C*
021800000801     C                   ENDSR
021900000801     C*----------------------------------------------------*
022000061031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
022100000801     C*----------------------------------------------------*
022200010201     C     IMPVAB        BEGSR
022300000801     C*
022400020305     C                   EXSR      INZVAR
022500020305     C                   EXSR      DEFCAM
022600150610     C*
022700150610     C* Se il rcd inizia con INSERT si prende in considerazione
022800150610     C                   IF        %subst(vindta:1:6) = 'INSERT'
022900061031     C*
023000061031     C* VALORIZZO CAMPI RELATIVI AL "CMR"
023100150610     C                   EVAL      VABCMR = 'PRADA'+%trim(%subst(vindta:249:4))
023200150610     C                                      + datora_A
023300061031     C                   EVAL      VABDCM = DATCOR
023400061031     C                   EVAL      VABDTS = DATCOR
023500061031     C                   EVAL      VABHMS = ORACOR
023600061031     C                   EVAL      VABCNT = 1
023700010305     C*
023800050202     C                   SETOFF                                       60
023900000801     C                   Z-ADD     *zeros        errore            1 0
024000000830     C                   MOVEL     datcor        VABAAS
024100020305     C                   MOVEL     datcor        VATAAS
024200021113     C                   MOVE(P)   vlrpoi        VABFGS
024300021113     C                   MOVE(P)   vlrpoi        VATFGS
024400010213     C                   MOVEL     VABAAS        depaas
024500000830     C                   MOVE      datcor        VABMGS
024600150610     C                   EVAL      w70=%trim(%subst(vindta:778:60))
024700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
024800150610     C     '@':'A'       XLATE     w70           w70
024900020117     C* ==
025000150610     C                   EVAL      VABRSD=%trim(%subst(w70:1:35))
025100150610     C                   EVAL      VABRD2=%trim(%subst(w70:36:35))
025200150610     C                   EVAL      VABIND=%trim(%subst(vindta:838:60))
025300150610     C                   EVAL      VABLOD=%trim(%subst(vindta:898:60))
025400150610     C                   EVAL      VABRMA=%trim(%subst(vindta:27:12))
025500150610     C                   EVAL      VABPRD=%trim(%subst(vindta:958:2))
025600150610     C                   EVAL      VABNAS=%trim(%subst(vindta:249:4))
025700020305     C*
025800150611     C* il numero spedizione l'ho reperito con la REPNSP ma la routine DEFCAM l'ha azzerato e quindi
025900150611     C* lo devo reimpostare (non lo fa per i campi di EDIVAT)
026000150611     C                   z-add     O33NRF        VABNSP
026100150611     C*
026200150610     C                   EVAL      PiStr=%trim(%subst(vindta:960:5))
026300000801     C                   EXSR      CHKNUM
026400000801     C                   IF        PiInt=*on
026500000801     C                   Z-ADD     PiVal         Num5_0
026600010208     C                   MOVEL(p)  Num5_0        VABCAD
026700000801     C                   ELSE
026800000801     C                   ADD       1             errore
026900000801     C                   EVAL      vinmsg = %trimr(vinmsg)
027000000801     C                             + ' ' + 'VABCAD'
027100000801     C                   ENDIF
027200020305     C*
027300150611     C                   EVAL      PiStr=%trim(%subst(vindta:426:5))
027400010201     C                   EXSR      CHKNUM
027500010201     C                   IF        PiInt=*on
027600010201     C                   Z-ADD     PiVal         Num5_0
027700010208     C                   MOVE      Num5_0        VABNCL
027800010201     C                   ELSE
027900010201     C                   ADD       1             errore
028000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
028100010201     C                             + ' ' + 'VABNCL'
028200010201     C                   ENDIF
028300020305     C*
028400150610     C                   EVAL      PiStr=%trim(%subst(vindta:1068:9))
028500010201     C                   EXSR      CHKNUM
028600010201     C                   IF        PiNum=*on
028700150610     C                   EVAL      VABPKB = PiVal/1000
028800010201     C                   ELSE
028900010201     C                   ADD       1             errore
029000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
029100010201     C                             + ' ' + 'VABPKB'
029200010201     C                   ENDIF
029300020305     C*
029400150610     C                   EVAL      PiStr=%trim(%subst(vindta:220:5))
029500010201     C                   EXSR      CHKNUM
029600010201     C                   IF        PiInt=*on
029700050511     C                   Z-ADD     PiVal         VABRMN
029800010201     C                   ELSE
029900050202     C                   SETON                                        60
030000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
030100150610     C                             + ' ' + 'VABRMN'
030200010201     C                   ENDIF
030300150610     C*
030400150610     C                   EVAL      PiStr=%trim(%subst(vindta:1163:9))
030500150610     C                   EXSR      CHKNUM
030600150610     C                   IF        PiInt=*on
030700150610     C                   EVAL      VABCCM = PiVal
030800150610     C                   ELSE
030900150610     C                   ADD       1             errore
031000150610     C                   EVAL      vinmsg = %trimr(vinmsg)
031100150610     C                             + ' ' + 'VABCCM'
031200150610     C                   ENDIF
031300010205     C*
031400010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
031500010205     C                   IF        VABCAS > *zeros
031600010205     C                   EVAL      VABCBO = '4'
031700010205     C                   ELSE
031800010205     C                   EVAL      VABCBO = '1'
031900010205     C                   ENDIF
032000020305     C*
032100011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
032200011113     C                   EXSR      CHKIMPDIV
032300150610     C*
032400150610     C                   ENDIF
032500010202     C*
032600000801     C* Ebbene...
032700050202     C                   IF        *IN60 = *ON
032800000801     C                   ADD       1             §CTRNO
032900000801     C                   EVAL      vinflg = '2'
033000000801     C                   ELSE
033100050202     C                   ADD       1             §CTROKVB
033200050202     C                   IF        errore <> *zeros
033300050202     C                   ADD       1             §CTRMO
033400050202     C                   ENDIF
033500000801     C                   ENDIF
033600000801     C*
033700000801     C                   ENDSR
033800010201     C*----------------------------------------------------*
033900061031     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
034000010201     C*----------------------------------------------------*
034100090403     C     WRIVAT_E      BEGSR
034200061031     C*
034300061031     C* VALORIZZO CAMPI RELATIVI AL "CMR"
034400061031     C                   EVAL      VATCMR = VABCMR
034500061031     C                   EVAL      VATCNT = VABCNT
034600010201     C*
034700090403     C* Valorizzo l buffer di scrittura del EDIVAT (x "CHI SONO")
034800090403     C                   MOVEL     'E'           VATTRC
034900150610     C                   eval      VATNOT = %subst(vindta:7:20)
035000061031     C                   write     EDIVAT00
035100010201     C*
035200010201     C                   ENDSR
035300090403     C*----------------------------------------------------*
035400090403     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
035500090403     C*----------------------------------------------------*
035600090403     C     WRIVAT_A      BEGSR
035700090403     C*
035800090403     C* VALORIZZO CAMPI RELATIVI AL "CMR"
035900090403     C                   EVAL      VATCMR = VABCMR
036000090403     C                   EVAL      VATCNT = VABCNT
036100090403     C*
036200090403     C* Valorizzo l buffer di scrittura del EDIVAT (x REFERENTE CONSEGNA)
036300090403     C                   MOVEL     'A'           VATTRC
036400090403     C                   eval      VATNOT = %subst(vindta:603+1:35-1)
036500090403     C                   write     EDIVAT00
036600090403     C*
036700090403     C                   ENDSR
036800090403     C*----------------------------------------------------*
036900090403     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
037000090403     C*----------------------------------------------------*
037100090403     C     WRIVAT_B      BEGSR
037200090403     C*
037300090403     C* VALORIZZO CAMPI RELATIVI AL "CMR"
037400090403     C                   EVAL      VATCMR = VABCMR
037500090403     C                   EVAL      VATCNT = VABCNT
037600090403     C*
037700090403     C* Valorizzo l buffer di scrittura del EDIVAT (x TELEFONO REFERENTE)
037800090403     C                   MOVEL     'B'           VATTRC
037900090403     C                   eval      VATNOT = %subst(vindta:498:24)
038000090403     C                   write     EDIVAT00
038100090403     C*
038200090403     C                   ENDSR
038300010202     C*----------------------------------------------------*
038400061031     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
038500010202     C*----------------------------------------------------*
038600020305     C     PREVAT        BEGSR
038700010202     C*
038800061031     C* Compongo il nome del membro da dare al EDIVATWR
038900010202     C                   eval      parmbr = vlrhdl
039000010202     C                   movel     'M'           parmbr
039100140205     C                   eval      parccm = '00288598'
039200010202     C                   eval      paropz = '1'
039300010202     C* Effettuo la chiamata al CLLE preposto
039400061031     C                   call(e)   'TITVEVTC'
039500010202     C                   parm                    parccm
039600010202     C                   parm                    parmbr
039700010202     C                   parm                    paropz
039800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
039900010202     C                   if        %error
040000010202     C                   movel     '1'           chkcall
040100010202     C                   else
040200010202     C                   movel     '0'           chkcall
040300010202     C                   endif
040400010202     C*
040500010202     C                   ENDSR
040600000801     C*----------------------------------------------------*
040700000801     C*  CONTROLLO NUMERICITA' CAMPI
040800000801     C*----------------------------------------------------*
040900000801     C     CHKNUM        BEGSR
041000000801     C*
041100000801     C                   call(e)   'ISNUMERIC'
041200000801     C                   PARM                    PiStr            30
041300000801     C                   PARM      ','           PiDecChr          1
041400000801     C                   PARM      *ZEROS        PiVal            30 9
041500000801     C                   PARM      '0'           PiInt             1
041600000801     C                   PARM      '0'           PiNum             1
041700000801     C                   IF        %error
041800000801     C                   EVAL      PiInt=*off
041900000801     C                   ENDIF
042000000801     C*
042100000801     C                   ENDSR
042200000801     C***
042300000801
042400011113
042500011113     C*----------------------------------------------------*
042600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
042700011113     C*----------------------------------------------------*
042800011113     C     CHKIMPDIV     BEGSR
042900011113     C*
043000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
043100011113     C                   Z-ADD     *zeros        wrkDec            9 9
043200011113     C*
043300011113     C* Come prima cosa effettuo considerazioni sulla divisa
043400011113     C                   IF        vabIAS > *zeros
043500011113     C                   IF        vabVAS <> 'EUR'
043600011113     C                   EVAL      vabVAS =  'ITL'
043700011113     C                   ENDIF
043800011113     C                   ENDIF
043900011113     C*
044000011113     C                   IF        vabCAS > *zeros
044100011113     C                   IF        vabVCA <> 'EUR'
044200011113     C                   EVAL      vabVCA =  'ITL'
044300011113     C                   ENDIF
044400011113     C                   ENDIF
044500011113     C*
044600011113     C                   IF        vabVMD > *zeros
044700020305     C                   IF        vabVAD <> 'EUR'
044800011113     C                   EVAL      vabVAD =  'ITL'
044900011113     C                   ENDIF
045000011113     C                   ENDIF
045100011113     C*
045200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
045300011113     C                   Z-ADD     vabIAS        wrkDec
045400011113     C                   IF        wrkDec > *zeros
045500011113     C                   IF        vabVAS = 'ITL'
045600011113     C                   EVAL      vabIAS = *zeros
045700011113     C                   ENDIF
045800011113     C                   ENDIF
045900011113     C*
046000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
046100011113     C                   Z-ADD     vabCAS        wrkDec
046200011113     C                   IF        wrkDec > *zeros
046300011113     C                   IF        vabVCA = 'ITL'
046400011113     C                   EVAL      vabCAS = *zeros
046500011113     C                   ENDIF
046600011113     C                   ENDIF
046700011113     C*
046800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
046900011113     C                   Z-ADD     vabVMD        wrkDec
047000011113     C                   IF        wrkDec > *zeros
047100011113     C                   IF        vabVAD = 'ITL'
047200011113     C                   EVAL      vabVMD = *zeros
047300011113     C                   ENDIF
047400011113     C                   ENDIF
047500011113     C*
047600011113     C                   ENDSR
047700011113     C***
047800011113
047900011113
048000000801
048100000801
048200990920      /TITLE Invio dei dati al punto operativo.
048300010202     C     invio         BEGSR
048400990920     C*
048500061031     C* 1° invio EDIVAT
048600010201     C                   reset                   dscmz
048700010201     C                   move      vlrpoi        cmzdst
048800061031     C                   eval      cmzfld = 'EDIVATWR'
048900010201     C                   eval      cmzmbd = vlrhdl
049000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
049100021009     C***                if        prmfir = *blanks
049200061031     C                   eval      cmzfla = 'EDIVAT0F'
049300061031     C                   eval      cmzmba = 'EDIVAT0F'
049400021009     C***                else
049500021009     C***                eval      cmzfla = prmfir
049600021009     C***                eval      cmzmba = prmfir
049700021009     C***                endif
049800010201     C                   eval      cmznrr = *zeros
049900020305     C                   move      §ctrokvt      cmznrr
050000021018     C                   eval      cmzlba = vlrfl1
050100010201     C                   call(e)   'TIS711C'
050200010201     C                   parm                    dscmz
050300010201     C                   parm      *blanks       esito
050400010205     C                   if        %error
050500010205     C                             or cmzerr = '1'
050600010205     C                             or esito  = '1'
050700010205     C                   eval      wrkesito = '3'
050800010205     C                   else
050900010201     C*
051000061031     C* 2° invio EDIVAB
051100010201     C                   reset                   dscmz
051200010201     C                   move      vlrpoi        cmzdst
051300010201     C                   eval      cmzfld = vlrfou
051400010201     C                   eval      cmzmbd = vlrhdl
051500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
051600021009     C***                if        prmfir = *blanks
051700061031     C                   eval      cmzfla = 'EDIVAB0F'
051800061031     C                   eval      cmzmba = 'EDIVAB0F'
051900021009     C***                else
052000021009     C***                eval      cmzfla = prmfir
052100021009     C***                eval      cmzmba = prmfir
052200021009     C***                endif
052300010201     C                   eval      cmznrr = *zeros
052400010201     C                   move      §ctrokvb      cmznrr
052500021018     C                   eval      cmzlba = vlrfl1
052600010201     C                   call(e)   'TIS711C'
052700010201     C                   parm                    dscmz
052800010201     C                   parm      *blanks       esito
052900010201     C                   if        %error
053000010201     C                             or cmzerr = '1'
053100010201     C                             or esito  = '1'
053200010201     C                   eval      wrkesito = '3'
053300010201     C                   endif
053400010205     C                   endif
053500990920     C*
053600000613     C                   ENDSR
053700150611
053800150611     C*----------------------------------------------------*
053900150611     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
054000150611     C*----------------------------------------------------*
054100150611     C     REPNSP        BEGSR
054200150611     C*
054300150611     C* NSP => Stacco un numeratore da AZNUM
054400150611     C                   clear                   TRUL33DS
054500150611     C                   eval      I33OPE = *zeros
054600150611     C                   eval      I33CNU = 302
054700150611     C                   eval      I33NUM = 1
054800150611     C                   movel     TRUL33DS      KPJBU
054900150611     C                   call      'TRUL33R'
055000150611     C                   parm                    KPJBA
055100150611     C                   movel     KPJBU         TRUL33DS
055200150611     C                   if        O33ERR = *zeros
055300150611     C                   z-add     O33NRF        VABNSP
055400150611     C                   z-add     O33NRF        VATNSP
055500150611     C                   else
055600150611     C                   SETON                                        31
055700150611     C                   EVAL      vinmsg = %trimr(vinmsg)
055800150611     C                             + ' ' + 'VABNSP VATNSP'
055900150611     C                   endif
056000150611     C*
056100150611     C                   ENDSR
056200000613     C***
056300070411
056400070411     C     *pssr         BEGSR
056500070411     C*
056600070411     C                   if        %open(tivin00r)
056700070411     C                   close     tivin00r
056800070411     C                   endif
056900070502     C                   if        %open(edivabwr)
057000070502     C                   close     edivabwr
057100070411     C                   endif
057200070502     C                   if        %open(edivatwr)
057300070502     C                   close     edivatwr
057400070411     C                   endif
057500070411     C*
057600070411     C* Effettuo la chiamata al CLLE preposto
057700070502     C                   call(e)   'TITVEVTC'
057800070502     C                   parm                    parccm
057900070502     C                   parm                    parmbr
058000070502     C                   parm      '2'           paropz
058100070411     C*
058200070411     C                   eval      wrkesito = '2'
058300070411     C*
058400070411     C                   seton                                        LR
058500070411     C*
058600070411     C                   ENDSR     '*CANCL'
058700070411     C***
058800070411
058900990910
059000000613     C     *inzsr        BEGSR
059100990910     C*
059200990910     C     *entry        plist
059300990920     C                   parm                    tivlrds
059400990921     C                   parm      wrkesito      esito
059500000724     C                   parm                    prmlit
059600000710     C                   parm                    prmfir
059700000613     C*
059800000830     C* CALCOLA LA DATA CORRENTE
059900000830     C                   time                    wn14             14 0
060000000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
060100061031     C                   movel     wn14          oracor            6 0          *ORA
060200000830     C                   z-add     wn8           g08dat
060300000830     C                   z-add     *zeros        g08inv
060400000830     C                   movel     '0'           g08err
060500000830     C                   call      'XSRDA8'
060600000830     C                   parm                    wlbda8
060700000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
060800061031     C*
060900061031     C* Compongo data/ora in formato alfa
061000061031     C                   movel(P)  datcor        datora_A         14
061100061031     C                   move      oracor        datora_A
061200000830     C*
061300000613     C                   ENDSR
061400000613     C***
