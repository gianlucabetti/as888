000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021113     FFIVABwwr  O    E             DISK    usropn
000600021113     FFIVATwwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500070719     D tisi95ds      e ds
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100010202     D parccm          s              8    INZ(*blanks)
002200010202     D parmbr          s             10    INZ(*blanks)
002300010202     D paropz          s              1    INZ(*blanks)
002400010202     D chkcall         s              1    INZ(*blanks)
002500000830
002600041025     D*------------------
002700041025     D* DS REPERIMENTO NUMERATORE
002800041025     D*------------------
002900041025     D trul33ds      e ds                  inz
003000041025     D*------------------
003100041025     D* DS ARCHITETTURA
003200041025     D*------------------
003300041025     D kpjba         e ds                  inz
003400041025     D*------------------
003500990908
003600010201
003700010201
003800000913     C                   reset                   rrnum
003900990921     C                   reset                   esito
004000990921     C                   reset                   wrkesito
004100000613     C*
004200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004300070719     C*
004400070719     C* Esegue lancio TISI95R solo x chiusura
004500070719     C                   CLEAR                   TISI95DS
004600070719     C                   EVAL      I95TLA = 'C'
004700070719     C                   CALL      'TISI95R'
004800070719     C                   PARM                    TISI95DS
004900000613     C*
005000010202     C* Effettuo la chiamata al CLLE preposto
005100040506     C                   call(e)   'TITVVTC'
005200010202     C                   parm                    parccm
005300010202     C                   parm                    parmbr
005400010202     C                   parm      '2'           paropz
005500000616     C*
005600010201     C                   seton                                        LR
005700990908
005800000801
005900910830     C*--------------------------------------------------------
006000040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006100910830     C*--------------------------------------------------------
006200040526     C     RWFILE        BEGSR
006300990910     C*
006400990914     C                   if        not %open(tivin00r)
006500990908     C                   open      tivin00r
006600990914     C                   endif
006700021113     C                   if        not %open(fivabwwr)
006800021113     C                   open      fivabwwr
006900990914     C                   endif
007000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007100020305     C                   exsr      prevat
007200010201     C*
007300010202     C                   if        chkcall = '0'
007400010202     C*
007500021113     C                   if        not %open(fivatwwr)
007600021113     C                   open      fivatwwr
007700010201     C                   endif
007800990910     C*
007900010201     C                   clear                   §CTROKVB          5 0
008000020305     C                   clear                   §CTROKVT          5 0
008100000801     C                   clear                   §CTRMO            5 0
008200000801     C                   clear                   §CTRNO            5 0
008300100729     C*
008400100729     C                   z-add     1             wGiro             1 0
008500130708     C*
008600130708     C                   SETOFF                                       31
008700921023     C                   DO        *HIVAL
008800990913     C*
008900990915     C                   READ      tivin00r                               70
009000040910     C                   if        vindta > *blanks
009100000613     C                   add       1             rrnum
009200000801     C*
009300000801     C                   if        *in70 = *off
009400000801     C                             and
009500000801     C                             (vinflg = *blanks
009600000801     C                              or vinflg = '0'
009700000801     C                              or vinflg = '2')
009800000801     C*
009900000801     C                   clear                   vinmsg
010000000801     C                   eval      vinflg = '1'
010100040910     C*
010200040910     C* Eseguo routine d traduzione
010300040910     C                   exsr      impvabvat
010400040802     C*
010500010305     C                   endif
010600000905     C*
010700000905     C                   else
010800000905     C                   eval      vinflg = '1'
010900000905     C                   endif
011000000905     C*
011100000905     C  N70              update    tivin000
011200000905     C*
011300991022     C  N70              ENDdo
011400100722     C*
011500100722     C* Scarico i buffer testata ancora "in canna"
011600130708     C  N31              WRITE     FIVAB000
011700130708     C                   SETOFF                                       31
011800010202     C*
011900010202     C                   endif
012000990910
012100990910     C* Se non ci sono record con errori ...
012200000710     C                   if        §ctrno = 0
012300990910     C* ... restituisco esito OK.
012400990921     C                   eval      wrkesito = '0'
012500990910     C                   else
012600010201     C                   if        §ctrokvb > 0
012700990921     C                   eval      wrkesito = '1'
012800000710     C                   else
012900000710     C                   eval      wrkesito = '2'
013000990910     C                   endif
013100000710     C                   endif
013200990910     C*
013300990914     C                   if        %open(tivin00r)
013400990908     C                   close     tivin00r
013500990914     C                   endif
013600021113     C                   if        %open(fivabwwr)
013700021113     C                   close     fivabwwr
013800990914     C                   endif
013900021113     C                   if        %open(fivatwwr)
014000021113     C                   close     fivatwwr
014100010201     C                   endif
014200990910     C*
014300010201     C                   if        §ctrokvb > 0
014400000724     C                             and vlrpoi <> *zeros
014500010202     C                   exsr      invio
014600990920     C                   endif
014700990920     C*
014800910830     C                   ENDSR
014900000613     C***
015000990920
015100000801     C*----------------------------------------------------*
015200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
015300000801     C*----------------------------------------------------*
015400010201     C     INZVAR        BEGSR
015500000801     C*
015600040802     C                   Z-ADD     *zeros        Num5_0            5 0
015700040802     C                   MOVEL     '0'           FlgCAS            1
015800000801     C*
015900000801     C                   ENDSR
016000000801     C*----------------------------------------------------*
016100040910     C*  IMPOSTAZIONE CAMPI COSTANTI
016200000801     C*----------------------------------------------------*
016300000801     C     DEFCAM        BEGSR
016400000801     C*
016500021113     C                   CLEAR                   FIVAB000
016600040802     C                   CLEAR                   FIVAT000
016700020619     C* Imposto i valori di default...
016800130620     C                   Z-ADD     0897206       VABCCM
016900130620     C                   Z-ADD     0897206       VATCCM
017000130620     C                   Z-ADD     089           VABLNP
017100130620     C                   Z-ADD     089           VATLNP
017200070531     C                   Z-ADD     000           VABCTR
017300070531     C                   MOVEL     '7Q'          VABCTM
017400040823     C                   MOVEL     '1'           VABCBO
017500020619     C* ... e poi verifico se sono stati passati come parametri
017600020619     C                   IF        vlrppt > *blanks
017700040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
017800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
017900020619     C                   EXSR      CHKNUM
018000020619     C                   IF        PiInt=*on
018100020619     C                   Z-ADD     PiVal         VABCCM
018200020619     C                   Z-ADD     PiVal         VATCCM
018300020619     C                   ENDIF
018400040506     C                   ENDIF
018500040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
018600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
018700020619     C                   EXSR      CHKNUM
018800020619     C                   IF        PiInt=*on
018900020619     C                   Z-ADD     PiVal         VABLNP
019000020619     C                   Z-ADD     PiVal         VATLNP
019100040506     C                   ENDIF
019200020619     C                   ENDIF
019300040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
019400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
019500020619     C                   EXSR      CHKNUM
019600020619     C                   IF        PiInt=*on
019700020619     C                   Z-ADD     PiVal         VABCTR
019800040506     C                   ENDIF
019900020619     C                   ENDIF
020000060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
020100060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
020200060202     C                   ENDIF
020300020619     C                   ENDIF
020400000801     C*
020500000801     C                   ENDSR
020600000801     C*----------------------------------------------------*
020700040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
020800000801     C*----------------------------------------------------*
020900040910     C     IMPVABVAT     BEGSR
021000040910     C*
021100040910     C* Traduzione relativa ai tipi record del file del cliente
021200040910     C*
021300071210     C*
021400071210     C***
021500130620     C* ...tipo record '@@PH' (info CMR)
021600130620     C                   IF        %subst(vindta:1:4) = '@@PH'
021700130620     C                   MOVEL     *blanks       wRMA             15
021800130620     C                   EVAL      wRMA = %trim(%subst(vindta:13:13))
021900130620     C                   ENDIF
022000130620     C***
022100130620     C* ...tipo record '1' (info testata)
022200130620     C                   IF        %subst(vindta:1:1) = '1'
022300100722     C*
022400100722     C* Se nn primo giro => scarico il buffer precedente
022500100722     C                   if        wGiro = 1
022600100722     C                   eval      wGiro = 2
022700100722     C                   else
022800130708     C  N31              WRITE     FIVAB000
022900130708     C                   SETOFF                                       31
023000100722     C                   endif
023100100722     C*
023200071210     C* Resetto indicatore d anomalia sul singolo record
023300071210     C                   eval      vinflg = '1'
023400071210     C* ......inizializzazioni iniziali e formati record file Bartolini
023500071210     C                   EXSR      INZVAR
023600071210     C                   EXSR      DEFCAM
023700071210     C*
023800071210     C                   Z-ADD     *zeros        errore            1 0
023900071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
024000071210     C                   MOVEL     datcor        VABAAS
024100071210     C                   MOVEL     datcor        VATAAS
024200071210     C                   MOVE      datcor        VABMGS
024300071210     C                   MOVE(P)   vlrpoi        VABFGS
024400071210     C                   MOVE(P)   vlrpoi        VATFGS
024500071210     C* ......VABNSP/VATNSP
024600071210     C* NSP => Stacco un numeratore da AZNUM
024700071210     C                   clear                   TRUL33DS
024800071210     C                   eval      I33OPE = *zeros
024900071210     C                   eval      I33CNU = 302
025000071210     C                   eval      I33NUM = 1
025100071210     C                   movel     TRUL33DS      KPJBU
025200071210     C                   call      'TRUL33R'
025300071210     C                   parm                    KPJBA
025400071210     C                   movel     KPJBU         TRUL33DS
025500071210     C                   if        O33ERR = *zeros
025600071210     C                   z-add     O33NRF        VABNSP
025700071210     C                   z-add     O33NRF        VATNSP
025800071210     C                   else
025900071210     C                   Z-ADD     1             errore
026000071210     C                   EVAL      vinmsg = %trimr(vinmsg)
026100071210     C                             + ' ' + 'VABNSP VATNSP'
026200071210     C                   endif
026300060202     C* ......VABRMN
026400130620     C                   EVAL      PiStr=%trim(%subst(vindta:8:10))
026500060202     C                   EXSR      CHKNUM
026600060202     C                   IF        PiInt=*on
026700060202     C                   Z-ADD     PiVal         VABRMN
026800060202     C                   ELSE
026900060202     C                   ADD       1             errore
027000060202     C                   EVAL      vinmsg = %trimr(vinmsg)
027100060202     C                             + ' ' + 'VABRMN'
027200060202     C                   ENDIF
027300110211     C* ......VABRMA
027400130624     C                   EVAL      VABRMA=%trim(%subst(vindta:8:10))
027500060202     C* ......VABRSD
027600130620     C                   EVAL      VABRSD=%trim(%subst(vindta:21:30))
027700130620     C* ......VABRD2
027800130620     C                   EVAL      VABRD2=%trim(%subst(vindta:130:35))
027900100714     C* ......VABIND
028000130620     C                   EVAL      VABIND=%trim(%subst(vindta:51:30))
028100130620     C* ......VABCAD
028200130620     C                   EVAL      VABCAD=%trim(%subst(vindta:81:7))
028300060202     C* ......VABLOD
028400130620     C                   EVAL      VABLOD=%trim(%subst(vindta:88:22))
028500130620     C* ......VABNZD
028600130620     C                   EVAL      VABNZD=%trim(%subst(vindta:110:3))
028700130620     C                   IF        VABNZD='IT'
028800130620     C                   EVAL      VABNZD=*blanks
028900130620     C                   ENDIF
029000130708     C                   IF        VABNZD<>*blanks
029100130708     C                   SETON                                        31
029200130708     C                   ENDIF
029300060202     C* ......VABPRD
029400070719     C* Reperisco la provincia dal CAP e dalla località
029500070719     C                   IF        VABPRD  = *blanks AND
029600130620     C                             VABLOD <> *blanks AND
029700130620     C                             VABCAD <> *blanks AND
029800130620     C                             VABNZD  = *blanks
029900070719     C                   CLEAR                   TISI95DS
030000070719     C                   EVAL      I95TCN = '3'
030100070719     C                   Z-ADD     datcor        I95DAT
030200070719     C                   EVAL      I95CAP = VABCAD
030300070719     C                   EVAL      I95LOC = VABLOD
030400070719     C                   CALL      'TISI95R'
030500070719     C                   PARM                    TISI95DS
030600070719     C                   EVAL      VABPRD = O95PRV
030700070719     C                   ENDIF
030800060202     C*
030900060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
031000060202     C                   IF        FlgCAS <> '0'
031100060202     C                   IF        VABCBO = '1'
031200060202     C                   EVAL      VABCBO = '4'
031300060202     C                   ELSE
031400060202     C                   EVAL      VABCBO = '6'
031500060202     C                   ENDIF
031600060202     C                   ENDIF
031700060202     C*
031800060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
031900060202     C                   EXSR      CHKIMPDIV
032000040910     C*
032100040910     C                   ENDIF
032200110425     C***
032300130620     C* ...tipo record '2' (info dettaglio colli)
032400130620     C                   IF        %subst(vindta:1:1) = '2'
032500110425     C* Resetto indicatore d anomalia sul singolo record
032600110425     C                   eval      vinflg = '1'
032700130620     C* ......VABNCL
032800130620     C                   ADD       1             VABNCL
032900130620     C* ......VABPKB
033000130620     C                   EVAL      PiStr=%trim(%subst(vindta:102:9))
033100140220     C                   EVAL      PiStr = %replace(',':PiStr:%scan('.': PiStr)
033200140220     C                                      :1)
033300130620     C                   EXSR      CHKNUM
033400130620     C                   IF        PiNum=*on
033500130620     C                   ADD       PiVal         VABPKB
033600130620     C                   ELSE
033700130620     C                   ADD       1             errore
033800130620     C                   EVAL      vinmsg = %trimr(vinmsg)
033900130620     C                             + ' ' + 'VABPKB'
034000130620     C                   ENDIF
034100130620     C* ......VATNOT_E
034200130620     C                   EVAL      VATNOT=%trim(%subst(vindta:2:20))
034300100722     C                   EVAL      VATTRC = 'E'
034400110425     C* x ogni tipo record '109' devo scaricare il buffer del file FIVAT
034500100722     C                   IF        VATNOT <> *blanks
034600100722     C                   ADD       1             §CTROKVT
034700100722     C                   WRITE     FIVAT000
034800100722     C                   ENDIF
034900100722     C*
035000100722     C                   ENDIF
035100010202     C*
035200000801     C* Ebbene...
035300000801     C                   ADD       1             §CTRMO
035400010201     C                   IF        errore <> *zeros
035500000801     C                   ADD       1             §CTRNO
035600000801     C                   EVAL      vinflg = '2'
035700000801     C                   ELSE
035800010201     C                   ADD       1             §CTROKVB
035900000801     C                   ENDIF
036000000801     C*
036100000801     C                   ENDSR
036200010202     C*----------------------------------------------------*
036300021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
036400010202     C*----------------------------------------------------*
036500020305     C     PREVAT        BEGSR
036600010202     C*
036700021113     C* Compongo il nome del membro da dare al FIVATWWR
036800010202     C                   eval      parmbr = vlrhdl
036900010202     C                   movel     'M'           parmbr
037000060113     C                   eval      parccm = vlrksc
037100010202     C                   eval      paropz = '1'
037200010202     C* Effettuo la chiamata al CLLE preposto
037300040506     C                   call(e)   'TITVVTC'
037400010202     C                   parm                    parccm
037500010202     C                   parm                    parmbr
037600010202     C                   parm                    paropz
037700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
037800010202     C                   if        %error
037900010202     C                   movel     '1'           chkcall
038000010202     C                   else
038100010202     C                   movel     '0'           chkcall
038200010202     C                   endif
038300010202     C*
038400010202     C                   ENDSR
038500000801     C*----------------------------------------------------*
038600000801     C*  CONTROLLO NUMERICITA' CAMPI
038700000801     C*----------------------------------------------------*
038800000801     C     CHKNUM        BEGSR
038900000801     C*
039000000801     C                   call(e)   'ISNUMERIC'
039100000801     C                   PARM                    PiStr            30
039200040714     C                   PARM      ','           PiDecChr          1
039300000801     C                   PARM      *ZEROS        PiVal            30 9
039400000801     C                   PARM      '0'           PiInt             1
039500000801     C                   PARM      '0'           PiNum             1
039600000801     C                   IF        %error
039700000801     C                   EVAL      PiInt=*off
039800000801     C                   ENDIF
039900000801     C*
040000000801     C                   ENDSR
040100000801     C***
040200000801
040300011113
040400011113     C*----------------------------------------------------*
040500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
040600011113     C*----------------------------------------------------*
040700011113     C     CHKIMPDIV     BEGSR
040800011113     C*
040900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
041000011113     C                   Z-ADD     *zeros        wrkDec            9 9
041100011113     C*
041200011113     C* Come prima cosa effettuo considerazioni sulla divisa
041300011113     C                   IF        vabIAS > *zeros
041400011113     C                   IF        vabVAS <> 'EUR'
041500011113     C                   EVAL      vabVAS =  'ITL'
041600011113     C                   ENDIF
041700011113     C                   ENDIF
041800011113     C*
041900011113     C                   IF        vabCAS > *zeros
042000011113     C                   IF        vabVCA <> 'EUR'
042100011113     C                   EVAL      vabVCA =  'ITL'
042200011113     C                   ENDIF
042300011113     C                   ENDIF
042400011113     C*
042500011113     C                   IF        vabVMD > *zeros
042600020305     C                   IF        vabVAD <> 'EUR'
042700011113     C                   EVAL      vabVAD =  'ITL'
042800011113     C                   ENDIF
042900011113     C                   ENDIF
043000011113     C*
043100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
043200011113     C                   Z-ADD     vabIAS        wrkDec
043300011113     C                   IF        wrkDec > *zeros
043400011113     C                   IF        vabVAS = 'ITL'
043500011113     C                   EVAL      vabIAS = *zeros
043600011113     C                   ENDIF
043700011113     C                   ENDIF
043800011113     C*
043900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
044000011113     C                   Z-ADD     vabCAS        wrkDec
044100011113     C                   IF        wrkDec > *zeros
044200011113     C                   IF        vabVCA = 'ITL'
044300011113     C                   EVAL      vabCAS = *zeros
044400011113     C                   ENDIF
044500011113     C                   ENDIF
044600011113     C*
044700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
044800011113     C                   Z-ADD     vabVMD        wrkDec
044900011113     C                   IF        wrkDec > *zeros
045000011113     C                   IF        vabVAD = 'ITL'
045100011113     C                   EVAL      vabVMD = *zeros
045200011113     C                   ENDIF
045300011113     C                   ENDIF
045400011113     C*
045500011113     C                   ENDSR
045600011113     C***
045700011113
045800011113
045900000801
046000000801
046100990920      /TITLE Invio dei dati al punto operativo.
046200010202     C     invio         BEGSR
046300990920     C*
046400021113     C* 1° invio FIVAT
046500010201     C                   reset                   dscmz
046600010201     C                   move      vlrpoi        cmzdst
046700021113     C                   eval      cmzfld = 'FIVATWWR'
046800010201     C                   eval      cmzmbd = vlrhdl
046900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
047000021009     C***                if        prmfir = *blanks
047100021113     C                   eval      cmzfla = 'FIVAT00F'
047200021113     C                   eval      cmzmba = 'FIVAT00F'
047300021009     C***                else
047400021009     C***                eval      cmzfla = prmfir
047500021009     C***                eval      cmzmba = prmfir
047600021009     C***                endif
047700010201     C                   eval      cmznrr = *zeros
047800020305     C                   move      §ctrokvt      cmznrr
047900021018     C                   eval      cmzlba = vlrfl1
048000010201     C                   call(e)   'TIS711C'
048100010201     C                   parm                    dscmz
048200010201     C                   parm      *blanks       esito
048300010205     C                   if        %error
048400010205     C                             or cmzerr = '1'
048500010205     C                             or esito  = '1'
048600010205     C                   eval      wrkesito = '3'
048700010205     C                   else
048800010201     C*
048900021113     C* 2° invio FIVAB
049000010201     C                   reset                   dscmz
049100010201     C                   move      vlrpoi        cmzdst
049200010201     C                   eval      cmzfld = vlrfou
049300010201     C                   eval      cmzmbd = vlrhdl
049400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
049500021009     C***                if        prmfir = *blanks
049600021113     C                   eval      cmzfla = 'FIVAB00F'
049700021113     C                   eval      cmzmba = 'FIVAB00F'
049800021009     C***                else
049900021009     C***                eval      cmzfla = prmfir
050000021009     C***                eval      cmzmba = prmfir
050100021009     C***                endif
050200010201     C                   eval      cmznrr = *zeros
050300010201     C                   move      §ctrokvb      cmznrr
050400021018     C                   eval      cmzlba = vlrfl1
050500010201     C                   call(e)   'TIS711C'
050600010201     C                   parm                    dscmz
050700010201     C                   parm      *blanks       esito
050800010201     C                   if        %error
050900010201     C                             or cmzerr = '1'
051000010201     C                             or esito  = '1'
051100010201     C                   eval      wrkesito = '3'
051200010201     C                   endif
051300010205     C                   endif
051400990920     C*
051500000613     C                   ENDSR
051600000613     C***
051700070411
051800070411     C     *pssr         BEGSR
051900070411     C*
052000070411     C                   if        %open(tivin00r)
052100070411     C                   close     tivin00r
052200070411     C                   endif
052300070411     C                   if        %open(fivabwwr)
052400070411     C                   close     fivabwwr
052500070411     C                   endif
052600070411     C                   if        %open(fivatwwr)
052700070411     C                   close     fivatwwr
052800070411     C                   endif
052900070411     C*
053000070411     C* Effettuo la chiamata al CLLE preposto
053100070411     C                   call(e)   'TITVVTC'
053200070411     C                   parm                    parccm
053300070411     C                   parm                    parmbr
053400070411     C                   parm      '2'           paropz
053500070411     C*
053600070411     C                   eval      wrkesito = '2'
053700070411     C*
053800070411     C                   seton                                        LR
053900070411     C*
054000070411     C                   ENDSR     '*CANCL'
054100070411     C***
054200070411
054300990910
054400000613     C     *inzsr        BEGSR
054500990910     C*
054600990910     C     *entry        plist
054700990920     C                   parm                    tivlrds
054800990921     C                   parm      wrkesito      esito
054900000724     C                   parm                    prmlit
055000000710     C                   parm                    prmfir
055100000613     C*
055200000830     C* CALCOLA LA DATA CORRENTE
055300100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
055400100722     C                   eval      datcor = %dec(%date() : *ISO)
055500000830     C*
055600000613     C                   ENDSR
055700000613     C***
