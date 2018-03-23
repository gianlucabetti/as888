000100101105      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500101105     FEDIVABwr  O    E             DISK    usropn
000600101105     FEDIVATwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500990910     D esito           s              1
001600000724     D prmlit          s             10
001700000710     D prmfir          s             10
001800990921     D wrkesito        s                   like(esito)
001900000613     D rrnum           s              6  0 INZ(*zeros)
002000010202     D parccm          s              8    INZ(*blanks)
002100010202     D parmbr          s             10    INZ(*blanks)
002200010202     D paropz          s              1    INZ(*blanks)
002300010202     D chkcall         s              1    INZ(*blanks)
002400010213
002500000801     D  Num5_0         s              5  0
002600030107     D  Num7_0         s              7  0
002700000830
002800000830     D*------------------
002900000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003000000830     D*------------------
003100000830     D WLBDA8          DS                  INZ
003200000830     D  G08DAT                 1      8  0
003300000830     D  G08INV                 9     16  0
003400000830     D  G08ERR                17     17
003500000830     D  G08TGI                18     22  0
003600000830     D*
003700990908
003800010201
003900010201
004000000913     C                   reset                   rrnum
004100990921     C                   reset                   esito
004200990921     C                   reset                   wrkesito
004300000613     C*
004400030226     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004500000613     C*
004600010202     C* Effettuo la chiamata al CLLE preposto
004700101105     C                   call(e)   'TITVEVTC'
004800010202     C                   parm                    parccm
004900010202     C                   parm                    parmbr
005000010202     C                   parm      '2'           paropz
005100000616     C*
005200010201     C                   seton                                        LR
005300990908
005400000801
005500910830     C*--------------------------------------------------------
005600101105     C* RWFILE LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR    *
005700910830     C*--------------------------------------------------------
005800030226     C     RWFILE        BEGSR
005900990910     C*
006000990914     C                   if        not %open(tivin00r)
006100990908     C                   open      tivin00r
006200990914     C                   endif
006300101105     C                   if        not %open(edivabwr)
006400101105     C                   open      edivabwr
006500990914     C                   endif
006600101105     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
006700020305     C                   exsr      prevat
006800010201     C*
006900010202     C                   if        chkcall = '0'
007000010202     C*
007100101105     C                   if        not %open(edivatwr)
007200101105     C                   open      edivatwr
007300010201     C                   endif
007400990910     C*
007500010201     C                   clear                   §CTROKVB          5 0
007600020305     C                   clear                   §CTROKVT          5 0
007700000801     C                   clear                   §CTRMO            5 0
007800000801     C                   clear                   §CTRNO            5 0
007900990910     C*
008000921023     C                   DO        *HIVAL
008100990913     C*
008200990915     C                   READ      tivin00r                               70
008300000905     C                   if        vindta > *blanks
008400000613     C                   add       1             rrnum
008500000801     C*
008600000801     C                   if        *in70 = *off
008700000801     C                             and
008800000801     C                             (vinflg = *blanks
008900000801     C                              or vinflg = '0'
009000000801     C                              or vinflg = '2')
009100000801     C*
009200000801     C                   clear                   vinmsg
009300030310     C                   eval      vinflg = '1'
009400030411     C*
009500101105     C                   clear                   edivab00
009600101105     C                   clear                   edivat00
009700030411     C                   exsr      impvab                                       => carico VAB
009800030411     C                   exsr      impvat                                       => carico VAT
009900030411     C                   exsr      wrivab                                       => scarico VAB
010000030411     C                   exsr      wrivat                                       => scarico VAT
010100000905     C*
010200000905     C                   else
010300000905     C                   eval      vinflg = '1'
010400000905     C                   endif
010500030411     C                   endif
010600000905     C*
010700000905     C  N70              update    tivin000
010800000905     C*
010900991022     C  N70              ENDdo
011000010202     C*
011100010202     C                   endif
011200990910
011300990910     C* Se non ci sono record con errori ...
011400000710     C                   if        §ctrno = 0
011500990910     C* ... restituisco esito OK.
011600990921     C                   eval      wrkesito = '0'
011700990910     C                   else
011800010201     C                   if        §ctrokvb > 0
011900990921     C                   eval      wrkesito = '1'
012000000710     C                   else
012100000710     C                   eval      wrkesito = '2'
012200990910     C                   endif
012300000710     C                   endif
012400990910     C*
012500990914     C                   if        %open(tivin00r)
012600990908     C                   close     tivin00r
012700990914     C                   endif
012800101105     C                   if        %open(edivabwr)
012900101105     C                   close     edivabwr
013000990914     C                   endif
013100101105     C                   if        %open(edivatwr)
013200101105     C                   close     edivatwr
013300010201     C                   endif
013400021017     C*
013500021017     C                   if        vlrpoi <> 999
013600021017     C                   eval      vlrpoi = vlrpoi
013700021017     C                   endif
013800990910     C*
013900010201     C                   if        §ctrokvb > 0
014000021017     C                             and vlrpoi > *zeros
014100010202     C                   exsr      invio
014200990920     C                   endif
014300990920     C*
014400910830     C                   ENDSR
014500000613     C***
014600990920
014700000801     C*----------------------------------------------------*
014800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
014900000801     C*----------------------------------------------------*
015000010201     C     INZVAR        BEGSR
015100000801     C*
015200010201     C                   Z-ADD     *zeros        Num5_0
015300030107     C                   Z-ADD     *zeros        Num7_0
015400030310     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
015500030310     C                   MOVEL     '0'           FlgCAS            1
015600000801     C*
015700000801     C                   ENDSR
015800000801     C*----------------------------------------------------*
015900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
016000000801     C*----------------------------------------------------*
016100000801     C     DEFCAM        BEGSR
016200000801     C*
016300020619     C* Imposto i valori di default...
016400030415     C                   Z-ADD     0492593       VABCCM
016500030415     C                   Z-ADD     0492593       VATCCM
016600021017     C                   Z-ADD     vlrpoi        VABFGS
016700021017     C                   Z-ADD     vlrpoi        VATFGS
016800030415     C                   Z-ADD     049           VABLNP
016900030415     C                   Z-ADD     049           VATLNP
017000101210     C                   Z-ADD     000           VABCTR
017100021017     C                   MOVEL     '7Q'          VABCTM
017200020305     C                   MOVEL     'E'           VATTRC
017300020619     C* ... e poi verifico se sono stati passati come parametri
017400020619     C                   IF        vlrppt > *blanks
017500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
017600020619     C                   EXSR      CHKNUM
017700020619     C                   IF        PiInt=*on
017800020619     C                   Z-ADD     PiVal         VABCCM
017900020619     C                   Z-ADD     PiVal         VATCCM
018000020619     C                   ENDIF
018100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
018200020619     C                   EXSR      CHKNUM
018300020619     C                   IF        PiInt=*on
018400020619     C                   Z-ADD     PiVal         VABLNP
018500020619     C                   Z-ADD     PiVal         VATLNP
018600020619     C                   ENDIF
018700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
018800020619     C                   EXSR      CHKNUM
018900020619     C                   IF        PiInt=*on
019000020619     C                   Z-ADD     PiVal         VABCTR
019100020619     C                   ENDIF
019200020619     C                   ENDIF
019300000801     C*
019400000801     C                   ENDSR
019500000801     C*----------------------------------------------------*
019600030415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
019700000801     C*----------------------------------------------------*
019800010201     C     IMPVAB        BEGSR
019900000801     C*
020000020305     C                   EXSR      INZVAR
020100020305     C                   EXSR      DEFCAM
020200010305     C*
020300030226     C                   Z-ADD     *zeros        errore            1 0
020400000830     C                   MOVEL     datcor        VABAAS
020500000830     C                   MOVE      datcor        VABMGS
020600030226     C*
020700030415     C                   EVAL      VABRSD=%trim(%subst(vindta:32:30))
020800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
020900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
021000020117     C* ==
021100030415     C                   IF        %subst(vindta:62:3) = 'IT'
021200030415     C                   ELSE
021300030415     C                   EVAL      VABNZD=%trim(%subst(vindta:62:3))
021400030415     C                   ENDIF
021500030415     C                   EVAL      VABIND=%trim(%subst(vindta:65:30))
021600030415     C                   EVAL      VABLOD=%trim(%subst(vindta:155:30))
021700030415     C                   EVAL      VABPRD=%trim(%subst(vindta:95:30))
021800030415     C                   EVAL      VABRMA=%trim(%subst(vindta:22:10))
021900030311     C                   EVAL      VABVCA='EUR'
022000030226     C* CAP
022100030415     C                   EVAL      PiStr=%trim(%subst(vindta:185:10))
022200010201     C                   EXSR      CHKNUM
022300010201     C                   IF        PiInt=*on
022400010201     C                   Z-ADD     PiVal         Num5_0
022500021017     C                   MOVEL     Num5_0        VABCAD
022600010201     C                   ELSE
022700010201     C                   ADD       1             errore
022800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
022900021017     C                             + ' ' + 'VABCAD'
023000010201     C                   ENDIF
023100030311     C* NSP
023200060104     C***                EVAL      PiStr=%trim(%subst(vindta:7:7))
023300060104     C                   EVAL      PiStr=%trim(%subst(vindta:14+1:8-1))
023400010201     C                   EXSR      CHKNUM
023500010201     C                   IF        PiInt=*on
023600030107     C                   Z-ADD     PiVal         Num7_0
023700030107     C                   MOVE(P)   Num7_0        VABNSP
023800030107     C                   MOVE(P)   Num7_0        VATNSP
023900010201     C                   ELSE
024000010201     C                   ADD       1             errore
024100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
024200030311     C                             + ' ' + 'VABNSP VATNSP'
024300010201     C                   ENDIF
024400060104     C* RMN
024500060104     C***                EVAL      PiStr=%trim(%subst(vindta:1:13))
024600060104     C                   EVAL      PiStr=%trim(%subst(vindta:14:8))
024700030311     C                   EXSR      CHKNUM
024800030311     C                   IF        PiInt=*on
024900030311     C                   Z-ADD     PiVal         VABRMN
025000030311     C                   ELSE
025100030311     C                   ADD       1             errore
025200030311     C                   EVAL      vinmsg = %trimr(vinmsg)
025300030311     C                             + ' ' + 'VABRMN'
025400030311     C                   ENDIF
025500030226     C* NCL
025600030415     C                   EVAL      VABNCL = VABNCL + 1
025700030226     C* PKB
025800030415     C                   EVAL      PiStr=%trim(%subst(vindta:203:7))
025900030107     C                   EXSR      CHKNUM
026000030107     C                   IF        PiNum=*on
026100030415     C                   ADD       PiVal         VABPKB
026200030107     C                   ELSE
026300030107     C                   ADD       1             errore
026400030107     C                   EVAL      vinmsg = %trimr(vinmsg)
026500030107     C                             + ' ' + 'VABPKB'
026600030107     C                   ENDIF
026700030415     C* CAS
026800030415     C                   IF        %subst(vindta:210:13) <> '0000000000,00'
026900030310     C                   MOVEL     '1'           FlgCAS
027000101117     C                   EVAL      VABTIC = 'BM'
027100101117     C***                EVAL      VABTIC = 'OM'
027200101117     C***                EVAL      VABRMO = 'YAMAHA MOTOR ITALIA SPA'
027300101117     C***                EVAL      VABCMO = '20050'
027400030415     C                   EVAL      PiStr=%trim(%subst(vindta:210:13))
027500030310     C                   EXSR      CHKNUM
027600030310     C                   IF        PiNum=*on
027700030714     C                   ADD       PiVal         VABCAS
027800030310     C                   ELSE
027900030310     C                   SETON                                        32
028000030310     C                   Z-ADD     *zeros        VABCAS
028100030310     C                   EVAL      vinmsg = %trimr(vinmsg)
028200030310     C                             + ' ' + 'VABCAS'
028300030310     C                   ENDIF
028400030310     C                   ENDIF
028500030310     C*
028600030310     C* Considerazioni finali su CBO/CAS
028700030310     C                   IF        FlgCAS = '1'
028800010205     C                   EVAL      VABCBO = '4'
028900010205     C                   ELSE
029000010205     C                   EVAL      VABCBO = '1'
029100010205     C                   ENDIF
029200020305     C*
029300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
029400011113     C                   EXSR      CHKIMPDIV
029500010202     C*
029600000801     C* Ebbene...
029700000801     C                   ADD       1             §CTRMO
029800010201     C                   IF        errore <> *zeros
029900000801     C                   ADD       1             §CTRNO
030000000801     C                   EVAL      vinflg = '2'
030100000801     C                   ELSE
030200010201     C                   ADD       1             §CTROKVB
030300000801     C                   ENDIF
030400000801     C*
030500000801     C                   ENDSR
030600030226     C*----------------------------------------------------*
030700030226     C*  SCARICAMENTO BUFFER RECORDS VAB
030800030226     C*----------------------------------------------------*
030900030226     C     WRIVAB        BEGSR
031000101105     C*
031100101105     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
031200101105     C                   EVAL      VABCMR = %subst(wNomeFile:6:5)
031300101105     C                   EVAL      VABDCM = DATCOR
031400101105     C                   EVAL      VABDTS = DATCOR
031500101105     C                   EVAL      VABHMS = ORACOR
031600101105     C                   EVAL      VABCNT = 1
031700030310     C*
031800101105     C                   write     EDIVAB00
031900030226     C*
032000030226     C                   ENDSR
032100030226     C*----------------------------------------------------*
032200030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
032300030226     C*----------------------------------------------------*
032400030226     C     IMPVAT        BEGSR
032500030226     C*
032600030226     C                   EXSR      INZVAR
032700030226     C                   EXSR      DEFCAM
032800030226     C*
032900030226     C                   MOVEL     datcor        VATAAS
033000030226     C*
033100030415     C* Valorizzo il barcode
033200030415     C                   EVAL      VATNOT=%trim(%subst(vindta:14:8))
033300030226     C*
033400030226     C                   ENDSR
033500010201     C*----------------------------------------------------*
033600021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
033700010201     C*----------------------------------------------------*
033800020305     C     WRIVAT        BEGSR
033900101105     C*
034000101105     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
034100101105     C                   EVAL      VATCMR = %subst(wNomeFile:6:5)
034200101105     C                   EVAL      VATCNT = 1
034300010201     C*
034400101105     C                   write     EDIVAT00
034500010201     C*
034600010201     C                   ENDSR
034700010202     C*----------------------------------------------------*
034800101105     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
034900010202     C*----------------------------------------------------*
035000020305     C     PREVAT        BEGSR
035100010202     C*
035200101105     C* Compongo il nome del membro da dare al EDIVATWR
035300010202     C                   eval      parmbr = vlrhdl
035400010202     C                   movel     'M'           parmbr
035500051230     C                   eval      parccm = vlrksc
035600010202     C                   eval      paropz = '1'
035700010202     C* Effettuo la chiamata al CLLE preposto
035800101105     C                   call(e)   'TITVEVTC'
035900010202     C                   parm                    parccm
036000010202     C                   parm                    parmbr
036100010202     C                   parm                    paropz
036200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
036300010202     C                   if        %error
036400010202     C                   movel     '1'           chkcall
036500010202     C                   else
036600010202     C                   movel     '0'           chkcall
036700010202     C                   endif
036800010202     C*
036900010202     C                   ENDSR
037000000801     C*----------------------------------------------------*
037100000801     C*  CONTROLLO NUMERICITA' CAMPI
037200000801     C*----------------------------------------------------*
037300000801     C     CHKNUM        BEGSR
037400000801     C*
037500000801     C                   call(e)   'ISNUMERIC'
037600000801     C                   PARM                    PiStr            30
037700030107     C                   PARM      ','           PiDecChr          1
037800000801     C                   PARM      *ZEROS        PiVal            30 9
037900000801     C                   PARM      '0'           PiInt             1
038000000801     C                   PARM      '0'           PiNum             1
038100000801     C                   IF        %error
038200000801     C                   EVAL      PiInt=*off
038300030107     C                   EVAL      PiNum=*off
038400000801     C                   ENDIF
038500000801     C*
038600000801     C                   ENDSR
038700000801     C***
038800000801
038900011113
039000011113     C*----------------------------------------------------*
039100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
039200011113     C*----------------------------------------------------*
039300011113     C     CHKIMPDIV     BEGSR
039400011113     C*
039500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
039600011113     C                   Z-ADD     *zeros        wrkDec            9 9
039700011113     C*
039800011113     C* Come prima cosa effettuo considerazioni sulla divisa
039900011113     C                   IF        vabIAS > *zeros
040000011113     C                   IF        vabVAS <> 'EUR'
040100011113     C                   EVAL      vabVAS =  'ITL'
040200011113     C                   ENDIF
040300011113     C                   ENDIF
040400011113     C*
040500011113     C                   IF        vabCAS > *zeros
040600011113     C                   IF        vabVCA <> 'EUR'
040700011113     C                   EVAL      vabVCA =  'ITL'
040800011113     C                   ENDIF
040900011113     C                   ENDIF
041000011113     C*
041100011113     C                   IF        vabVMD > *zeros
041200020305     C                   IF        vabVAD <> 'EUR'
041300011113     C                   EVAL      vabVAD =  'ITL'
041400011113     C                   ENDIF
041500011113     C                   ENDIF
041600011113     C*
041700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
041800011113     C                   Z-ADD     vabIAS        wrkDec
041900011113     C                   IF        wrkDec > *zeros
042000011113     C                   IF        vabVAS = 'ITL'
042100011113     C                   EVAL      vabIAS = *zeros
042200011113     C                   ENDIF
042300011113     C                   ENDIF
042400011113     C*
042500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
042600011113     C                   Z-ADD     vabCAS        wrkDec
042700011113     C                   IF        wrkDec > *zeros
042800011113     C                   IF        vabVCA = 'ITL'
042900011113     C                   EVAL      vabCAS = *zeros
043000011113     C                   ENDIF
043100011113     C                   ENDIF
043200011113     C*
043300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
043400011113     C                   Z-ADD     vabVMD        wrkDec
043500011113     C                   IF        wrkDec > *zeros
043600011113     C                   IF        vabVAD = 'ITL'
043700011113     C                   EVAL      vabVMD = *zeros
043800011113     C                   ENDIF
043900011113     C                   ENDIF
044000011113     C*
044100011113     C                   ENDSR
044200011113     C***
044300021017
044400000801
044500000801
044600990920      /TITLE Invio dei dati al punto operativo.
044700010202     C     invio         BEGSR
044800990920     C*
044900021017     C* 1° invio FIVAT
045000010201     C                   reset                   dscmz
045100021017     C                   move      vlrpoi        cmzdst
045200101105     C                   eval      cmzfld = 'EDIVATWR'
045300010201     C                   eval      cmzmbd = vlrhdl
045400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
045500021009     C***                if        prmfir = *blanks
045600101105     C                   eval      cmzfla = 'EDIVAT0F'
045700101105     C                   eval      cmzmba = 'EDIVAT0F'
045800021009     C***                else
045900021009     C***                eval      cmzfla = prmfir
046000021009     C***                eval      cmzmba = prmfir
046100021009     C***                endif
046200010201     C                   eval      cmznrr = *zeros
046300020305     C                   move      §ctrokvt      cmznrr
046400021018     C                   eval      cmzlba = vlrfl1
046500010201     C                   call(e)   'TIS711C'
046600010201     C                   parm                    dscmz
046700010201     C                   parm      *blanks       esito
046800010205     C                   if        %error
046900010205     C                             or cmzerr = '1'
047000010205     C                             or esito  = '1'
047100010205     C                   eval      wrkesito = '3'
047200010205     C                   else
047300010201     C*
047400021017     C* 2° invio FIVAB
047500010201     C                   reset                   dscmz
047600021017     C                   move      vlrpoi        cmzdst
047700010201     C                   eval      cmzfld = vlrfou
047800010201     C                   eval      cmzmbd = vlrhdl
047900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
048000021009     C***                if        prmfir = *blanks
048100101105     C                   eval      cmzfla = 'EDIVAB0F'
048200101105     C                   eval      cmzmba = 'EDIVAB0F'
048300021009     C***                else
048400021009     C***                eval      cmzfla = prmfir
048500021009     C***                eval      cmzmba = prmfir
048600021009     C***                endif
048700010201     C                   eval      cmznrr = *zeros
048800010201     C                   move      §ctrokvb      cmznrr
048900021018     C                   eval      cmzlba = vlrfl1
049000010201     C                   call(e)   'TIS711C'
049100010201     C                   parm                    dscmz
049200010201     C                   parm      *blanks       esito
049300010201     C                   if        %error
049400010201     C                             or cmzerr = '1'
049500010201     C                             or esito  = '1'
049600010201     C                   eval      wrkesito = '3'
049700010201     C                   endif
049800010205     C                   endif
049900990920     C*
050000000613     C                   ENDSR
050100000613     C***
050200070411
050300070411     C     *pssr         BEGSR
050400070411     C*
050500070411     C                   if        %open(tivin00r)
050600070411     C                   close     tivin00r
050700070411     C                   endif
050800101105     C                   if        %open(edivabwr)
050900101105     C                   close     edivabwr
051000070411     C                   endif
051100101105     C                   if        %open(edivatwr)
051200101105     C                   close     edivatwr
051300070411     C                   endif
051400070411     C*
051500070411     C* Effettuo la chiamata al CLLE preposto
051600101105     C                   call(e)   'TITVEVTC'
051700070411     C                   parm                    parccm
051800070411     C                   parm                    parmbr
051900070411     C                   parm      '2'           paropz
052000070411     C*
052100070411     C                   eval      wrkesito = '2'
052200070411     C*
052300070411     C                   seton                                        LR
052400070411     C*
052500070411     C                   ENDSR     '*CANCL'
052600070411     C***
052700070411
052800990910
052900000613     C     *inzsr        BEGSR
053000990910     C*
053100990910     C     *entry        plist
053200990920     C                   parm                    tivlrds
053300990921     C                   parm      wrkesito      esito
053400000724     C                   parm                    prmlit
053500000710     C                   parm                    prmfir
053600000613     C*
053700000830     C* CALCOLA LA DATA CORRENTE
053800000830     C                   time                    wn14             14 0
053900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
054000101105     C                   movel     wn14          oracor            6 0          *ORA
054100000830     C                   z-add     wn8           g08dat
054200000830     C                   z-add     *zeros        g08inv
054300000830     C                   movel     '0'           g08err
054400000830     C                   call      'XSRDA8'
054500000830     C                   parm                    wlbda8
054600000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
054700101105     C*
054800101105     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
054900101105     C                   move(p)   vlrMSG        wNomeFile        30
055000000830     C*
055100000613     C                   ENDSR
055200000613     C***
