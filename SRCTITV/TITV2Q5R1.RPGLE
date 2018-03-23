000100150427      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500150427     FEDIVABwr  O    E             DISK    usropn
000600150427     FEDIVATwr  O    E             DISK    usropn
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
002500150427     D wNomeFile       s             30    INZ(*blanks)
002600000830
002700041025     D*------------------
002800041025     D* DS REPERIMENTO NUMERATORE
002900041025     D*------------------
003000041025     D trul33ds      e ds                  inz
003100041025     D*------------------
003200041025     D* DS ARCHITETTURA
003300041025     D*------------------
003400041025     D kpjba         e ds                  inz
003500041025     D*------------------
003600990908
003700010201
003800010201
003900000913     C                   reset                   rrnum
004000990921     C                   reset                   esito
004100990921     C                   reset                   wrkesito
004200000613     C*
004300040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004400070719     C*
004500070719     C* Esegue lancio TISI95R solo x chiusura
004600070719     C                   CLEAR                   TISI95DS
004700070719     C                   EVAL      I95TLA = 'C'
004800070719     C                   CALL      'TISI95R'
004900070719     C                   PARM                    TISI95DS
005000000613     C*
005100010202     C* Effettuo la chiamata al CLLE preposto
005200150427     C                   call(e)   'TITVEVTC'
005300010202     C                   parm                    parccm
005400010202     C                   parm                    parmbr
005500010202     C                   parm      '2'           paropz
005600000616     C*
005700010201     C                   seton                                        LR
005800990908
005900000801
006000910830     C*--------------------------------------------------------
006100150427     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
006200910830     C*--------------------------------------------------------
006300040526     C     RWFILE        BEGSR
006400990910     C*
006500990914     C                   if        not %open(tivin00r)
006600990908     C                   open      tivin00r
006700990914     C                   endif
006800150427     C                   if        not %open(edivabwr)
006900150427     C                   open      edivabwr
007000990914     C                   endif
007100150427     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
007200020305     C                   exsr      prevat
007300010201     C*
007400010202     C                   if        chkcall = '0'
007500010202     C*
007600150427     C                   if        not %open(edivatwr)
007700150427     C                   open      edivatwr
007800010201     C                   endif
007900990910     C*
008000010201     C                   clear                   §CTROKVB          5 0
008100020305     C                   clear                   §CTROKVT          5 0
008200000801     C                   clear                   §CTRMO            5 0
008300000801     C                   clear                   §CTRNO            5 0
008400100729     C*
008500100729     C                   z-add     1             wGiro             1 0
008600130708     C*
008700130708     C                   SETOFF                                       31
008800921023     C                   DO        *HIVAL
008900990913     C*
009000990915     C                   READ      tivin00r                               70
009100040910     C                   if        vindta > *blanks
009200000613     C                   add       1             rrnum
009300000801     C*
009400000801     C                   if        *in70 = *off
009500000801     C                             and
009600000801     C                             (vinflg = *blanks
009700000801     C                              or vinflg = '0'
009800000801     C                              or vinflg = '2')
009900000801     C*
010000000801     C                   clear                   vinmsg
010100000801     C                   eval      vinflg = '1'
010200040910     C*
010300040910     C* Eseguo routine d traduzione
010400040910     C                   exsr      impvabvat
010500040802     C*
010600010305     C                   endif
010700000905     C*
010800000905     C                   else
010900000905     C                   eval      vinflg = '1'
011000000905     C                   endif
011100000905     C*
011200000905     C  N70              update    tivin000
011300000905     C*
011400991022     C  N70              ENDdo
011500100722     C*
011600100722     C* Scarico i buffer testata ancora "in canna"
011700150427     C*
011800150427     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
011900150427     C                   MOVE(P)   vlrMSG        wNomeFile
012000150427     C                   EVAL      VABCMR=%char(datcor)+'-'+%char(oracor) +
012100150427     C                                    ' CMR:' +
012200150427     C                                    %subst(wNomeFile:5:3)
012300150427     C                   EVAL      VABDCM = datcor
012400150427     C                   EVAL      VABDTS = datcor
012500150427     C                   EVAL      VABHMS = oracor
012600150427     C                   EVAL      VABCNT = 1
012700150427     C*
012800150427     C  N31              WRITE     EDIVAB00
012900130708     C                   SETOFF                                       31
013000010202     C*
013100010202     C                   endif
013200990910
013300990910     C* Se non ci sono record con errori ...
013400000710     C                   if        §ctrno = 0
013500990910     C* ... restituisco esito OK.
013600990921     C                   eval      wrkesito = '0'
013700990910     C                   else
013800010201     C                   if        §ctrokvb > 0
013900990921     C                   eval      wrkesito = '1'
014000000710     C                   else
014100000710     C                   eval      wrkesito = '2'
014200990910     C                   endif
014300000710     C                   endif
014400990910     C*
014500990914     C                   if        %open(tivin00r)
014600990908     C                   close     tivin00r
014700990914     C                   endif
014800150427     C                   if        %open(edivabwr)
014900150427     C                   close     edivabwr
015000990914     C                   endif
015100150427     C                   if        %open(edivatwr)
015200150427     C                   close     edivatwr
015300010201     C                   endif
015400990910     C*
015500010201     C                   if        §ctrokvb > 0
015600000724     C                             and vlrpoi <> *zeros
015700010202     C                   exsr      invio
015800990920     C                   endif
015900990920     C*
016000910830     C                   ENDSR
016100000613     C***
016200990920
016300000801     C*----------------------------------------------------*
016400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
016500000801     C*----------------------------------------------------*
016600010201     C     INZVAR        BEGSR
016700000801     C*
016800040802     C                   Z-ADD     *zeros        Num5_0            5 0
016900040802     C                   MOVEL     '0'           FlgCAS            1
017000000801     C*
017100000801     C                   ENDSR
017200000801     C*----------------------------------------------------*
017300040910     C*  IMPOSTAZIONE CAMPI COSTANTI
017400000801     C*----------------------------------------------------*
017500000801     C     DEFCAM        BEGSR
017600000801     C*
017700150427     C                   CLEAR                   EDIVAB00
017800150427     C                   CLEAR                   EDIVAT00
017900020619     C* Imposto i valori di default...
018000130620     C                   Z-ADD     0897206       VABCCM
018100130620     C                   Z-ADD     0897206       VATCCM
018200130620     C                   Z-ADD     089           VABLNP
018300130620     C                   Z-ADD     089           VATLNP
018400070531     C                   Z-ADD     000           VABCTR
018500070531     C                   MOVEL     '7Q'          VABCTM
018600040823     C                   MOVEL     '1'           VABCBO
018700020619     C* ... e poi verifico se sono stati passati come parametri
018800020619     C                   IF        vlrppt > *blanks
018900040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
019000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
019100020619     C                   EXSR      CHKNUM
019200020619     C                   IF        PiInt=*on
019300020619     C                   Z-ADD     PiVal         VABCCM
019400020619     C                   Z-ADD     PiVal         VATCCM
019500020619     C                   ENDIF
019600040506     C                   ENDIF
019700040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
019800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
019900020619     C                   EXSR      CHKNUM
020000020619     C                   IF        PiInt=*on
020100020619     C                   Z-ADD     PiVal         VABLNP
020200020619     C                   Z-ADD     PiVal         VATLNP
020300040506     C                   ENDIF
020400020619     C                   ENDIF
020500040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
020600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
020700020619     C                   EXSR      CHKNUM
020800020619     C                   IF        PiInt=*on
020900020619     C                   Z-ADD     PiVal         VABCTR
021000040506     C                   ENDIF
021100020619     C                   ENDIF
021200060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
021300060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
021400060202     C                   ENDIF
021500020619     C                   ENDIF
021600000801     C*
021700000801     C                   ENDSR
021800000801     C*----------------------------------------------------*
021900150427     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
022000000801     C*----------------------------------------------------*
022100040910     C     IMPVABVAT     BEGSR
022200040910     C*
022300040910     C* Traduzione relativa ai tipi record del file del cliente
022400040910     C*
022500071210     C*
022600071210     C***
022700130620     C* ...tipo record '@@PH' (info CMR)
022800130620     C                   IF        %subst(vindta:1:4) = '@@PH'
022900130620     C                   MOVEL     *blanks       wRMA             15
023000130620     C                   EVAL      wRMA = %trim(%subst(vindta:13:13))
023100130620     C                   ENDIF
023200130620     C***
023300130620     C* ...tipo record '1' (info testata)
023400130620     C                   IF        %subst(vindta:1:1) = '1'
023500100722     C*
023600100722     C* Se nn primo giro => scarico il buffer precedente
023700100722     C                   if        wGiro = 1
023800100722     C                   eval      wGiro = 2
023900100722     C                   else
024000150427     C*
024100150427     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
024200150427     C                   MOVE(P)   vlrMSG        wNomeFile
024300150427     C                   EVAL      VABCMR=%char(datcor)+'-'+%char(oracor) +
024400150427     C                                    ' CMR:' +
024500150427     C                                    %subst(wNomeFile:5:3)
024600150427     C                   EVAL      VABDCM = datcor
024700150427     C                   EVAL      VABDTS = datcor
024800150427     C                   EVAL      VABHMS = oracor
024900150427     C                   EVAL      VABCNT = 1
025000150427     C*
025100150427     C  N31              WRITE     EDIVAB00
025200130708     C                   SETOFF                                       31
025300100722     C                   endif
025400100722     C*
025500071210     C* Resetto indicatore d anomalia sul singolo record
025600071210     C                   eval      vinflg = '1'
025700071210     C* ......inizializzazioni iniziali e formati record file Bartolini
025800071210     C                   EXSR      INZVAR
025900071210     C                   EXSR      DEFCAM
026000071210     C*
026100071210     C                   Z-ADD     *zeros        errore            1 0
026200071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
026300071210     C                   MOVEL     datcor        VABAAS
026400071210     C                   MOVEL     datcor        VATAAS
026500071210     C                   MOVE      datcor        VABMGS
026600071210     C                   MOVE(P)   vlrpoi        VABFGS
026700071210     C                   MOVE(P)   vlrpoi        VATFGS
026800071210     C* ......VABNSP/VATNSP
026900071210     C* NSP => Stacco un numeratore da AZNUM
027000071210     C                   clear                   TRUL33DS
027100071210     C                   eval      I33OPE = *zeros
027200071210     C                   eval      I33CNU = 302
027300071210     C                   eval      I33NUM = 1
027400071210     C                   movel     TRUL33DS      KPJBU
027500071210     C                   call      'TRUL33R'
027600071210     C                   parm                    KPJBA
027700071210     C                   movel     KPJBU         TRUL33DS
027800071210     C                   if        O33ERR = *zeros
027900071210     C                   z-add     O33NRF        VABNSP
028000071210     C                   z-add     O33NRF        VATNSP
028100071210     C                   else
028200071210     C                   Z-ADD     1             errore
028300071210     C                   EVAL      vinmsg = %trimr(vinmsg)
028400071210     C                             + ' ' + 'VABNSP VATNSP'
028500071210     C                   endif
028600060202     C* ......VABRMN
028700130620     C                   EVAL      PiStr=%trim(%subst(vindta:8:10))
028800060202     C                   EXSR      CHKNUM
028900060202     C                   IF        PiInt=*on
029000060202     C                   Z-ADD     PiVal         VABRMN
029100060202     C                   ELSE
029200060202     C                   ADD       1             errore
029300060202     C                   EVAL      vinmsg = %trimr(vinmsg)
029400060202     C                             + ' ' + 'VABRMN'
029500060202     C                   ENDIF
029600110211     C* ......VABRMA
029700130624     C                   EVAL      VABRMA=%trim(%subst(vindta:8:10))
029800060202     C* ......VABRSD
029900130620     C                   EVAL      VABRSD=%trim(%subst(vindta:21:30))
030000130620     C* ......VABRD2
030100130620     C                   EVAL      VABRD2=%trim(%subst(vindta:130:35))
030200100714     C* ......VABIND
030300130620     C                   EVAL      VABIND=%trim(%subst(vindta:51:30))
030400130620     C* ......VABCAD
030500130620     C                   EVAL      VABCAD=%trim(%subst(vindta:81:7))
030600060202     C* ......VABLOD
030700130620     C                   EVAL      VABLOD=%trim(%subst(vindta:88:22))
030800130620     C* ......VABNZD
030900130620     C                   EVAL      VABNZD=%trim(%subst(vindta:110:3))
031000130620     C                   IF        VABNZD='IT'
031100130620     C                   EVAL      VABNZD=*blanks
031200130620     C                   ENDIF
031300130708     C                   IF        VABNZD<>*blanks
031400130708     C                   SETON                                        31
031500130708     C                   ENDIF
031600060202     C* ......VABPRD
031700070719     C* Reperisco la provincia dal CAP e dalla località
031800070719     C                   IF        VABPRD  = *blanks AND
031900130620     C                             VABLOD <> *blanks AND
032000130620     C                             VABCAD <> *blanks AND
032100130620     C                             VABNZD  = *blanks
032200070719     C                   CLEAR                   TISI95DS
032300070719     C                   EVAL      I95TCN = '3'
032400070719     C                   Z-ADD     datcor        I95DAT
032500070719     C                   EVAL      I95CAP = VABCAD
032600070719     C                   EVAL      I95LOC = VABLOD
032700070719     C                   CALL      'TISI95R'
032800070719     C                   PARM                    TISI95DS
032900070719     C                   EVAL      VABPRD = O95PRV
033000070719     C                   ENDIF
033100060202     C*
033200060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
033300060202     C                   IF        FlgCAS <> '0'
033400060202     C                   IF        VABCBO = '1'
033500060202     C                   EVAL      VABCBO = '4'
033600060202     C                   ELSE
033700060202     C                   EVAL      VABCBO = '6'
033800060202     C                   ENDIF
033900060202     C                   ENDIF
034000060202     C*
034100060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
034200060202     C                   EXSR      CHKIMPDIV
034300040910     C*
034400040910     C                   ENDIF
034500110425     C***
034600130620     C* ...tipo record '2' (info dettaglio colli)
034700130620     C                   IF        %subst(vindta:1:1) = '2'
034800110425     C* Resetto indicatore d anomalia sul singolo record
034900110425     C                   eval      vinflg = '1'
035000130620     C* ......VABNCL
035100130620     C                   ADD       1             VABNCL
035200130620     C* ......VABPKB
035300130620     C                   EVAL      PiStr=%trim(%subst(vindta:102:9))
035400140220     C                   EVAL      PiStr = %replace(',':PiStr:%scan('.': PiStr)
035500140220     C                                      :1)
035600130620     C                   EXSR      CHKNUM
035700130620     C                   IF        PiNum=*on
035800130620     C                   ADD       PiVal         VABPKB
035900130620     C                   ELSE
036000130620     C                   ADD       1             errore
036100130620     C                   EVAL      vinmsg = %trimr(vinmsg)
036200130620     C                             + ' ' + 'VABPKB'
036300130620     C                   ENDIF
036400130620     C* ......VATNOT_E
036500130620     C                   EVAL      VATNOT=%trim(%subst(vindta:2:20))
036600100722     C                   EVAL      VATTRC = 'E'
036700150427     C* x ogni tipo record '109' devo scaricare il buffer del file EDIVAT
036800100722     C                   IF        VATNOT <> *blanks
036900100722     C                   ADD       1             §CTROKVT
037000150427     C*
037100150427     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
037200150427     C                   MOVE(P)   vlrMSG        wNomeFile
037300150427     C                   EVAL      VATCMR=%char(datcor)+'-'+%char(oracor) +
037400150427     C                                    ' CMR:' +
037500150427     C                                    %subst(wNomeFile:5:3)
037600150427     C                   EVAL      VATCNT = 1
037700150427     C*
037800150427     C                   WRITE     EDIVAT00
037900100722     C                   ENDIF
038000100722     C*
038100100722     C                   ENDIF
038200010202     C*
038300000801     C* Ebbene...
038400000801     C                   ADD       1             §CTRMO
038500010201     C                   IF        errore <> *zeros
038600000801     C                   ADD       1             §CTRNO
038700000801     C                   EVAL      vinflg = '2'
038800000801     C                   ELSE
038900010201     C                   ADD       1             §CTROKVB
039000000801     C                   ENDIF
039100000801     C*
039200000801     C                   ENDSR
039300010202     C*----------------------------------------------------*
039400150427     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
039500010202     C*----------------------------------------------------*
039600020305     C     PREVAT        BEGSR
039700010202     C*
039800150427     C* Compongo il nome del membro da dare al EDIVATWR
039900010202     C                   eval      parmbr = vlrhdl
040000010202     C                   movel     'M'           parmbr
040100060113     C                   eval      parccm = vlrksc
040200010202     C                   eval      paropz = '1'
040300010202     C* Effettuo la chiamata al CLLE preposto
040400150427     C                   call(e)   'TITVEVTC'
040500010202     C                   parm                    parccm
040600010202     C                   parm                    parmbr
040700010202     C                   parm                    paropz
040800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
040900010202     C                   if        %error
041000010202     C                   movel     '1'           chkcall
041100010202     C                   else
041200010202     C                   movel     '0'           chkcall
041300010202     C                   endif
041400010202     C*
041500010202     C                   ENDSR
041600000801     C*----------------------------------------------------*
041700000801     C*  CONTROLLO NUMERICITA' CAMPI
041800000801     C*----------------------------------------------------*
041900000801     C     CHKNUM        BEGSR
042000000801     C*
042100000801     C                   call(e)   'ISNUMERIC'
042200000801     C                   PARM                    PiStr            30
042300040714     C                   PARM      ','           PiDecChr          1
042400000801     C                   PARM      *ZEROS        PiVal            30 9
042500000801     C                   PARM      '0'           PiInt             1
042600000801     C                   PARM      '0'           PiNum             1
042700000801     C                   IF        %error
042800000801     C                   EVAL      PiInt=*off
042900000801     C                   ENDIF
043000000801     C*
043100000801     C                   ENDSR
043200000801     C***
043300000801
043400011113
043500011113     C*----------------------------------------------------*
043600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
043700011113     C*----------------------------------------------------*
043800011113     C     CHKIMPDIV     BEGSR
043900011113     C*
044000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
044100011113     C                   Z-ADD     *zeros        wrkDec            9 9
044200011113     C*
044300011113     C* Come prima cosa effettuo considerazioni sulla divisa
044400011113     C                   IF        vabIAS > *zeros
044500011113     C                   IF        vabVAS <> 'EUR'
044600011113     C                   EVAL      vabVAS =  'ITL'
044700011113     C                   ENDIF
044800011113     C                   ENDIF
044900011113     C*
045000011113     C                   IF        vabCAS > *zeros
045100011113     C                   IF        vabVCA <> 'EUR'
045200011113     C                   EVAL      vabVCA =  'ITL'
045300011113     C                   ENDIF
045400011113     C                   ENDIF
045500011113     C*
045600011113     C                   IF        vabVMD > *zeros
045700020305     C                   IF        vabVAD <> 'EUR'
045800011113     C                   EVAL      vabVAD =  'ITL'
045900011113     C                   ENDIF
046000011113     C                   ENDIF
046100011113     C*
046200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
046300011113     C                   Z-ADD     vabIAS        wrkDec
046400011113     C                   IF        wrkDec > *zeros
046500011113     C                   IF        vabVAS = 'ITL'
046600011113     C                   EVAL      vabIAS = *zeros
046700011113     C                   ENDIF
046800011113     C                   ENDIF
046900011113     C*
047000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
047100011113     C                   Z-ADD     vabCAS        wrkDec
047200011113     C                   IF        wrkDec > *zeros
047300011113     C                   IF        vabVCA = 'ITL'
047400011113     C                   EVAL      vabCAS = *zeros
047500011113     C                   ENDIF
047600011113     C                   ENDIF
047700011113     C*
047800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
047900011113     C                   Z-ADD     vabVMD        wrkDec
048000011113     C                   IF        wrkDec > *zeros
048100011113     C                   IF        vabVAD = 'ITL'
048200011113     C                   EVAL      vabVMD = *zeros
048300011113     C                   ENDIF
048400011113     C                   ENDIF
048500011113     C*
048600011113     C                   ENDSR
048700011113     C***
048800011113
048900011113
049000000801
049100000801
049200990920      /TITLE Invio dei dati al punto operativo.
049300010202     C     invio         BEGSR
049400990920     C*
049500150427     C* 1° invio EDIVAT
049600010201     C                   reset                   dscmz
049700010201     C                   move      vlrpoi        cmzdst
049800150427     C                   eval      cmzfld = 'EDIVATWR'
049900010201     C                   eval      cmzmbd = vlrhdl
050000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
050100021009     C***                if        prmfir = *blanks
050200150427     C                   eval      cmzfla = 'EDIVAT0F'
050300150427     C                   eval      cmzmba = 'EDIVAT0F'
050400021009     C***                else
050500021009     C***                eval      cmzfla = prmfir
050600021009     C***                eval      cmzmba = prmfir
050700021009     C***                endif
050800010201     C                   eval      cmznrr = *zeros
050900020305     C                   move      §ctrokvt      cmznrr
051000021018     C                   eval      cmzlba = vlrfl1
051100010201     C                   call(e)   'TIS711C'
051200010201     C                   parm                    dscmz
051300010201     C                   parm      *blanks       esito
051400010205     C                   if        %error
051500010205     C                             or cmzerr = '1'
051600010205     C                             or esito  = '1'
051700010205     C                   eval      wrkesito = '3'
051800010205     C                   else
051900010201     C*
052000150427     C* 2° invio EDIVAB
052100010201     C                   reset                   dscmz
052200010201     C                   move      vlrpoi        cmzdst
052300010201     C                   eval      cmzfld = vlrfou
052400010201     C                   eval      cmzmbd = vlrhdl
052500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
052600021009     C***                if        prmfir = *blanks
052700150427     C                   eval      cmzfla = 'EDIVAB0F'
052800150427     C                   eval      cmzmba = 'EDIVAB0F'
052900021009     C***                else
053000021009     C***                eval      cmzfla = prmfir
053100021009     C***                eval      cmzmba = prmfir
053200021009     C***                endif
053300010201     C                   eval      cmznrr = *zeros
053400010201     C                   move      §ctrokvb      cmznrr
053500021018     C                   eval      cmzlba = vlrfl1
053600010201     C                   call(e)   'TIS711C'
053700010201     C                   parm                    dscmz
053800010201     C                   parm      *blanks       esito
053900010201     C                   if        %error
054000010201     C                             or cmzerr = '1'
054100010201     C                             or esito  = '1'
054200010201     C                   eval      wrkesito = '3'
054300010201     C                   endif
054400010205     C                   endif
054500990920     C*
054600000613     C                   ENDSR
054700000613     C***
054800070411
054900070411     C     *pssr         BEGSR
055000070411     C*
055100070411     C                   if        %open(tivin00r)
055200070411     C                   close     tivin00r
055300070411     C                   endif
055400150427     C                   if        %open(edivabwr)
055500150427     C                   close     edivabwr
055600070411     C                   endif
055700150427     C                   if        %open(edivatwr)
055800150427     C                   close     edivatwr
055900070411     C                   endif
056000070411     C*
056100070411     C* Effettuo la chiamata al CLLE preposto
056200150427     C                   call(e)   'TITVEVTC'
056300070411     C                   parm                    parccm
056400070411     C                   parm                    parmbr
056500070411     C                   parm      '2'           paropz
056600070411     C*
056700070411     C                   eval      wrkesito = '2'
056800070411     C*
056900070411     C                   seton                                        LR
057000070411     C*
057100070411     C                   ENDSR     '*CANCL'
057200070411     C***
057300070411
057400990910
057500000613     C     *inzsr        BEGSR
057600990910     C*
057700990910     C     *entry        plist
057800990920     C                   parm                    tivlrds
057900990921     C                   parm      wrkesito      esito
058000000724     C                   parm                    prmlit
058100000710     C                   parm                    prmfir
058200000613     C*
058300000830     C* CALCOLA LA DATA CORRENTE
058400150427     C                   time                    wn14             14 0
058500150427     C                   movel     wn14          oracor            6 0          *ORA
058600100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
058700100722     C                   eval      datcor = %dec(%date() : *ISO)
058800000830     C*
058900000613     C                   ENDSR
059000000613     C***
