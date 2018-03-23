000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
002500130104     D w70             s             70    inz(*blanks)
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
005200040506     C                   call(e)   'TITVVTC'
005300010202     C                   parm                    parccm
005400010202     C                   parm                    parmbr
005500010202     C                   parm      '2'           paropz
005600000616     C*
005700010201     C                   seton                                        LR
005800990908
005900000801
006000910830     C*--------------------------------------------------------
006100040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006200910830     C*--------------------------------------------------------
006300040526     C     RWFILE        BEGSR
006400990910     C*
006500990914     C                   if        not %open(tivin00r)
006600990908     C                   open      tivin00r
006700990914     C                   endif
006800021113     C                   if        not %open(fivabwwr)
006900021113     C                   open      fivabwwr
007000990914     C                   endif
007100021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007200020305     C                   exsr      prevat
007300010201     C*
007400010202     C                   if        chkcall = '0'
007500010202     C*
007600021113     C                   if        not %open(fivatwwr)
007700021113     C                   open      fivatwwr
007800010201     C                   endif
007900990910     C*
008000010201     C                   clear                   §CTROKVB          5 0
008100020305     C                   clear                   §CTROKVT          5 0
008200000801     C                   clear                   §CTRMO            5 0
008300000801     C                   clear                   §CTRNO            5 0
008400100729     C*
008500100729     C                   z-add     1             wGiro             1 0
008600040910     C*
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
011600100722     C                   WRITE     FIVAB000
011700010202     C*
011800010202     C                   endif
011900990910
012000990910     C* Se non ci sono record con errori ...
012100000710     C                   if        §ctrno = 0
012200990910     C* ... restituisco esito OK.
012300990921     C                   eval      wrkesito = '0'
012400990910     C                   else
012500010201     C                   if        §ctrokvb > 0
012600990921     C                   eval      wrkesito = '1'
012700000710     C                   else
012800000710     C                   eval      wrkesito = '2'
012900990910     C                   endif
013000000710     C                   endif
013100990910     C*
013200990914     C                   if        %open(tivin00r)
013300990908     C                   close     tivin00r
013400990914     C                   endif
013500021113     C                   if        %open(fivabwwr)
013600021113     C                   close     fivabwwr
013700990914     C                   endif
013800021113     C                   if        %open(fivatwwr)
013900021113     C                   close     fivatwwr
014000010201     C                   endif
014100990910     C*
014200010201     C                   if        §ctrokvb > 0
014300000724     C                             and vlrpoi <> *zeros
014400010202     C                   exsr      invio
014500990920     C                   endif
014600990920     C*
014700910830     C                   ENDSR
014800000613     C***
014900990920
015000000801     C*----------------------------------------------------*
015100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
015200000801     C*----------------------------------------------------*
015300010201     C     INZVAR        BEGSR
015400000801     C*
015500040802     C                   Z-ADD     *zeros        Num5_0            5 0
015600040802     C                   MOVEL     '0'           FlgCAS            1
015700000801     C*
015800000801     C                   ENDSR
015900000801     C*----------------------------------------------------*
016000040910     C*  IMPOSTAZIONE CAMPI COSTANTI
016100000801     C*----------------------------------------------------*
016200000801     C     DEFCAM        BEGSR
016300000801     C*
016400021113     C                   CLEAR                   FIVAB000
016500040802     C                   CLEAR                   FIVAT000
016600020619     C* Imposto i valori di default...
016700130103     C                   Z-ADD     2221424       VABCCM
016800130103     C                   Z-ADD     2221424       VATCCM
016900130103     C                   Z-ADD     222           VABLNP
017000130103     C                   Z-ADD     222           VATLNP
017100070531     C                   Z-ADD     000           VABCTR
017200130103     C                   MOVEL     '7Q'          VABCTM
017300040823     C                   MOVEL     '1'           VABCBO
017400020619     C* ... e poi verifico se sono stati passati come parametri
017500020619     C                   IF        vlrppt > *blanks
017600040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
017700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
017800020619     C                   EXSR      CHKNUM
017900020619     C                   IF        PiInt=*on
018000020619     C                   Z-ADD     PiVal         VABCCM
018100020619     C                   Z-ADD     PiVal         VATCCM
018200020619     C                   ENDIF
018300040506     C                   ENDIF
018400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
018500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
018600020619     C                   EXSR      CHKNUM
018700020619     C                   IF        PiInt=*on
018800020619     C                   Z-ADD     PiVal         VABLNP
018900020619     C                   Z-ADD     PiVal         VATLNP
019000040506     C                   ENDIF
019100020619     C                   ENDIF
019200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
019300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
019400020619     C                   EXSR      CHKNUM
019500020619     C                   IF        PiInt=*on
019600020619     C                   Z-ADD     PiVal         VABCTR
019700040506     C                   ENDIF
019800020619     C                   ENDIF
019900060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
020000060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
020100060202     C                   ENDIF
020200020619     C                   ENDIF
020300000801     C*
020400000801     C                   ENDSR
020500000801     C*----------------------------------------------------*
020600040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
020700000801     C*----------------------------------------------------*
020800040910     C     IMPVABVAT     BEGSR
020900040910     C*
021000040910     C* Traduzione relativa ai tipi record del file del cliente
021100040910     C*
021200071210     C*
021300071210     C***
021400120716     C* ...tipo record '1' (testata documento)
021500130103     C                   IF        %subst(vindta:1:1) = 'S'
021600100722     C*
021700100722     C* Se nn primo giro => scarico il buffer precedente
021800100722     C                   if        wGiro = 1
021900100722     C                   eval      wGiro = 2
022000100722     C                   else
022100100722     C                   WRITE     FIVAB000
022200100722     C                   endif
022300100722     C*
022400071210     C* Resetto indicatore d anomalia sul singolo record
022500071210     C                   eval      vinflg = '1'
022600071210     C* ......inizializzazioni iniziali e formati record file Bartolini
022700071210     C                   EXSR      INZVAR
022800071210     C                   EXSR      DEFCAM
022900071210     C*
023000071210     C                   Z-ADD     *zeros        errore            1 0
023100071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
023200071210     C                   MOVEL     datcor        VABAAS
023300071210     C                   MOVEL     datcor        VATAAS
023400071210     C                   MOVE      datcor        VABMGS
023500071210     C                   MOVE(P)   vlrpoi        VABFGS
023600071210     C                   MOVE(P)   vlrpoi        VATFGS
023700071210     C* ......VABNSP/VATNSP
023800071210     C* NSP => Stacco un numeratore da AZNUM
023900071210     C                   clear                   TRUL33DS
024000071210     C                   eval      I33OPE = *zeros
024100071210     C                   eval      I33CNU = 302
024200071210     C                   eval      I33NUM = 1
024300071210     C                   movel     TRUL33DS      KPJBU
024400071210     C                   call      'TRUL33R'
024500071210     C                   parm                    KPJBA
024600071210     C                   movel     KPJBU         TRUL33DS
024700071210     C                   if        O33ERR = *zeros
024800071210     C                   z-add     O33NRF        VABNSP
024900071210     C                   z-add     O33NRF        VATNSP
025000071210     C                   else
025100071210     C                   Z-ADD     1             errore
025200071210     C                   EVAL      vinmsg = %trimr(vinmsg)
025300071210     C                             + ' ' + 'VABNSP VATNSP'
025400071210     C                   endif
025500130103     C* ......VABRMN / VABRMA
025600130103     C                   EVAL      VABRMN=%dec(%trim(%subst(vindta:2:15)):15:0)
025700130103     C                   EVAL      VABRMA=%trim(%subst(vindta:2:15))
025800120716     C* ......VABRSD
025900130103     C                   EVAL      VABRSD=%trim(%subst(vindta:161:30))
026000130103     C* ......VABRD2
026100130103     C                   EVAL      VABRD2=%trim(%subst(vindta:191:30))
026200130103     C* ......VABIND
026300130103     C                   EVAL      VABIND=%trim(%subst(vindta:221:30))
026400130103     C* ......VABCAD
026500130103     C                   EVAL      VABCAD=%trim(%subst(vindta:254:10))
026600120716     C* ......VABLOD
026700130103     C                   EVAL      VABLOD=%trim(%subst(vindta:264:30))
026800120716     C* ......VABPRD
026900130103     C                   EVAL      VABPRD=*blank
027000120716     C* Reperisco la provincia dal CAP e dalla località
027100120716     C                   IF        VABPRD  = *blanks AND
027200120716     C                             VABCAD <> *blanks
027300120716     C                   CLEAR                   TISI95DS
027400120716     C                   EVAL      I95TCN = '3'
027500120716     C                   Z-ADD     datcor        I95DAT
027600120716     C                   EVAL      I95CAP = VABCAD
027700120716     C                   EVAL      I95LOC = VABLOD
027800120716     C                   CALL      'TISI95R'
027900120716     C                   PARM                    TISI95DS
028000120716     C                   EVAL      VABPRD = O95PRV
028100120716     C                   ENDIF
028200130104     C* ......VABNOT/NT2
028300130104     C                   EVAL      w70=%trim(%subst(vindta:789:70))
028400130104     C                   EVAL      VABNOT=%subst(w70:01:35)
028500130104     C                   EVAL      VABNT2=%subst(w70:36:35)
028600130104     C* ......VABFFD
028700130104     C                   IF        %subst(vindta:786:3) = '313'
028800130104     C                   EVAL      VABFFD='S'
028900130104     C                   ENDIF
029000130104     C                   IF        %subst(vindta:859:3) = '313'
029100130104     C                   EVAL      VABFFD='S'
029200130104     C                   ENDIF
029300130104     C                   IF        %subst(vindta:932:3) = '313'
029400130104     C                   EVAL      VABFFD='S'
029500130104     C                   ENDIF
029600130104     C* ......VABTC1
029700130104     C                   IF        %subst(vindta:786:3) = '301'
029800130104     C                   EVAL      VABTC1='A'
029900130104     C                   ENDIF
030000130104     C                   IF        %subst(vindta:859:3) = '301'
030100130104     C                   EVAL      VABTC1='A'
030200130104     C                   ENDIF
030300130104     C                   IF        %subst(vindta:932:3) = '301'
030400130104     C                   EVAL      VABTC1='A'
030500130104     C                   ENDIF
030600110207     C* ......VABNCL
030700130103     C                   EVAL      PiStr=%trim(%subst(vindta:577:4))
030800100714     C                   EXSR      CHKNUM
030900100714     C                   IF        PiInt=*on
031000110211     C                   Z-ADD     PiVal         VABNCL
031100100714     C                   ELSE
031200100714     C                   ADD       1             errore
031300100714     C                   EVAL      vinmsg = %trimr(vinmsg)
031400100714     C                             + ' ' + 'VABNCL'
031500120716     C                   ENDIF
031600060202     C* ......VABPKB
031700130103     C                   EVAL      PiStr=%trim(%subst(vindta:549:5))
031800060202     C                   EXSR      CHKNUM
031900060202     C                   IF        PiNum=*on
032000130116     C***                EVAL      PiVal = PiVal / 10                           gest. 1 dec.
032100070726     C                   Z-ADD(H)  PiVal         VABPKB
032200060202     C                   ELSE
032300060202     C                   ADD       1             errore
032400060202     C                   EVAL      vinmsg = %trimr(vinmsg)
032500060202     C                             + ' ' + 'VABPKB'
032600060202     C                   ENDIF
032700130103     C* ......VABCAS
032800130103     C                   EVAL      VABVCA = *blank
032900130103     C                   IF        %trim(%subst(vindta:445:8))<>*blanks
033000130103     C                             AND
033100130103     C                             %trim(%subst(vindta:445:8))<>*zeros
033200130103     C                   MOVEL     '1'           FlgCAS
033300130103     C                   EVAL      PiStr=%trim(%subst(vindta:445:8))
033400130103     C                   EXSR      CHKNUM
033500130103     C                   IF        PiNum=*on
033600130103     C                   EVAL      PiVal = PiVal / 100                          gest. 2 dec.
033700130103     C                   Z-ADD     PiVal         VABCAS
033800130103     C                   EVAL      VABVCA = 'EUR'
033900130103     C                   ELSE
034000130103     C                   SETON                                        32
034100130103     C                   EVAL      vinmsg = %trimr(vinmsg)
034200130103     C                             + ' ' + 'VABCAS'
034300130103     C                   ENDIF
034400130103     C                   ENDIF
034500060202     C*
034600060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
034700060202     C                   IF        FlgCAS <> '0'
034800060202     C                   IF        VABCBO = '1'
034900060202     C                   EVAL      VABCBO = '4'
035000060202     C                   ELSE
035100060202     C                   EVAL      VABCBO = '6'
035200060202     C                   ENDIF
035300060202     C                   ENDIF
035400060202     C*
035500060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
035600060202     C                   EXSR      CHKIMPDIV
035700040910     C*
035800040910     C                   ENDIF
035900110425     C***
036000130103     C* ...tipo record 'P' (informazioni sui barcode)
036100130103     C                   IF        %subst(vindta:1:1) = 'P'
036200070719     C* Resetto indicatore d anomalia sul singolo record
036300070719     C                   eval      vinflg = '1'
036400100722     C* ......VATNOT
036500130103     C                   EVAL      VATNOT=%trim(%subst(vindta:693:20))
036600100722     C                   EVAL      VATTRC = 'E'
036700120716     C* x ogni tipo record '2' devo scaricare il buffer del file FIVAT
036800100722     C                   IF        VATNOT <> *blanks
036900100722     C                   ADD       1             §CTROKVT
037000100722     C                   WRITE     FIVAT000
037100100722     C                   ENDIF
037200100722     C*
037300100722     C                   ENDIF
037400010202     C*
037500000801     C* Ebbene...
037600000801     C                   ADD       1             §CTRMO
037700010201     C                   IF        errore <> *zeros
037800000801     C                   ADD       1             §CTRNO
037900000801     C                   EVAL      vinflg = '2'
038000000801     C                   ELSE
038100010201     C                   ADD       1             §CTROKVB
038200000801     C                   ENDIF
038300000801     C*
038400000801     C                   ENDSR
038500010202     C*----------------------------------------------------*
038600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
038700010202     C*----------------------------------------------------*
038800020305     C     PREVAT        BEGSR
038900010202     C*
039000021113     C* Compongo il nome del membro da dare al FIVATWWR
039100010202     C                   eval      parmbr = vlrhdl
039200010202     C                   movel     'M'           parmbr
039300060113     C                   eval      parccm = vlrksc
039400010202     C                   eval      paropz = '1'
039500010202     C* Effettuo la chiamata al CLLE preposto
039600040506     C                   call(e)   'TITVVTC'
039700010202     C                   parm                    parccm
039800010202     C                   parm                    parmbr
039900010202     C                   parm                    paropz
040000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
040100010202     C                   if        %error
040200010202     C                   movel     '1'           chkcall
040300010202     C                   else
040400010202     C                   movel     '0'           chkcall
040500010202     C                   endif
040600010202     C*
040700010202     C                   ENDSR
040800000801     C*----------------------------------------------------*
040900000801     C*  CONTROLLO NUMERICITA' CAMPI
041000000801     C*----------------------------------------------------*
041100000801     C     CHKNUM        BEGSR
041200000801     C*
041300000801     C                   call(e)   'ISNUMERIC'
041400000801     C                   PARM                    PiStr            30
041500040714     C                   PARM      ','           PiDecChr          1
041600000801     C                   PARM      *ZEROS        PiVal            30 9
041700000801     C                   PARM      '0'           PiInt             1
041800000801     C                   PARM      '0'           PiNum             1
041900000801     C                   IF        %error
042000000801     C                   EVAL      PiInt=*off
042100000801     C                   ENDIF
042200000801     C*
042300000801     C                   ENDSR
042400000801     C***
042500000801
042600011113
042700011113     C*----------------------------------------------------*
042800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
042900011113     C*----------------------------------------------------*
043000011113     C     CHKIMPDIV     BEGSR
043100011113     C*
043200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
043300011113     C                   Z-ADD     *zeros        wrkDec            9 9
043400011113     C*
043500011113     C* Come prima cosa effettuo considerazioni sulla divisa
043600011113     C                   IF        vabIAS > *zeros
043700011113     C                   IF        vabVAS <> 'EUR'
043800011113     C                   EVAL      vabVAS =  'ITL'
043900011113     C                   ENDIF
044000011113     C                   ENDIF
044100011113     C*
044200011113     C                   IF        vabCAS > *zeros
044300011113     C                   IF        vabVCA <> 'EUR'
044400011113     C                   EVAL      vabVCA =  'ITL'
044500011113     C                   ENDIF
044600011113     C                   ENDIF
044700011113     C*
044800011113     C                   IF        vabVMD > *zeros
044900020305     C                   IF        vabVAD <> 'EUR'
045000011113     C                   EVAL      vabVAD =  'ITL'
045100011113     C                   ENDIF
045200011113     C                   ENDIF
045300011113     C*
045400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
045500011113     C                   Z-ADD     vabIAS        wrkDec
045600011113     C                   IF        wrkDec > *zeros
045700011113     C                   IF        vabVAS = 'ITL'
045800011113     C                   EVAL      vabIAS = *zeros
045900011113     C                   ENDIF
046000011113     C                   ENDIF
046100011113     C*
046200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
046300011113     C                   Z-ADD     vabCAS        wrkDec
046400011113     C                   IF        wrkDec > *zeros
046500011113     C                   IF        vabVCA = 'ITL'
046600011113     C                   EVAL      vabCAS = *zeros
046700011113     C                   ENDIF
046800011113     C                   ENDIF
046900011113     C*
047000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
047100011113     C                   Z-ADD     vabVMD        wrkDec
047200011113     C                   IF        wrkDec > *zeros
047300011113     C                   IF        vabVAD = 'ITL'
047400011113     C                   EVAL      vabVMD = *zeros
047500011113     C                   ENDIF
047600011113     C                   ENDIF
047700011113     C*
047800011113     C                   ENDSR
047900011113     C***
048000011113
048100011113
048200000801
048300000801
048400990920      /TITLE Invio dei dati al punto operativo.
048500010202     C     invio         BEGSR
048600990920     C*
048700021113     C* 1° invio FIVAT
048800010201     C                   reset                   dscmz
048900010201     C                   move      vlrpoi        cmzdst
049000021113     C                   eval      cmzfld = 'FIVATWWR'
049100010201     C                   eval      cmzmbd = vlrhdl
049200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
049300021009     C***                if        prmfir = *blanks
049400021113     C                   eval      cmzfla = 'FIVAT00F'
049500021113     C                   eval      cmzmba = 'FIVAT00F'
049600021009     C***                else
049700021009     C***                eval      cmzfla = prmfir
049800021009     C***                eval      cmzmba = prmfir
049900021009     C***                endif
050000010201     C                   eval      cmznrr = *zeros
050100020305     C                   move      §ctrokvt      cmznrr
050200021018     C                   eval      cmzlba = vlrfl1
050300010201     C                   call(e)   'TIS711C'
050400010201     C                   parm                    dscmz
050500010201     C                   parm      *blanks       esito
050600010205     C                   if        %error
050700010205     C                             or cmzerr = '1'
050800010205     C                             or esito  = '1'
050900010205     C                   eval      wrkesito = '3'
051000010205     C                   else
051100010201     C*
051200021113     C* 2° invio FIVAB
051300010201     C                   reset                   dscmz
051400010201     C                   move      vlrpoi        cmzdst
051500010201     C                   eval      cmzfld = vlrfou
051600010201     C                   eval      cmzmbd = vlrhdl
051700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
051800021009     C***                if        prmfir = *blanks
051900021113     C                   eval      cmzfla = 'FIVAB00F'
052000021113     C                   eval      cmzmba = 'FIVAB00F'
052100021009     C***                else
052200021009     C***                eval      cmzfla = prmfir
052300021009     C***                eval      cmzmba = prmfir
052400021009     C***                endif
052500010201     C                   eval      cmznrr = *zeros
052600010201     C                   move      §ctrokvb      cmznrr
052700021018     C                   eval      cmzlba = vlrfl1
052800010201     C                   call(e)   'TIS711C'
052900010201     C                   parm                    dscmz
053000010201     C                   parm      *blanks       esito
053100010201     C                   if        %error
053200010201     C                             or cmzerr = '1'
053300010201     C                             or esito  = '1'
053400010201     C                   eval      wrkesito = '3'
053500010201     C                   endif
053600010205     C                   endif
053700990920     C*
053800000613     C                   ENDSR
053900000613     C***
054000070411
054100070411     C     *pssr         BEGSR
054200070411     C*
054300070411     C                   if        %open(tivin00r)
054400070411     C                   close     tivin00r
054500070411     C                   endif
054600070411     C                   if        %open(fivabwwr)
054700070411     C                   close     fivabwwr
054800070411     C                   endif
054900070411     C                   if        %open(fivatwwr)
055000070411     C                   close     fivatwwr
055100070411     C                   endif
055200070411     C*
055300070411     C* Effettuo la chiamata al CLLE preposto
055400070411     C                   call(e)   'TITVVTC'
055500070411     C                   parm                    parccm
055600070411     C                   parm                    parmbr
055700070411     C                   parm      '2'           paropz
055800070411     C*
055900070411     C                   eval      wrkesito = '2'
056000070411     C*
056100070411     C                   seton                                        LR
056200070411     C*
056300070411     C                   ENDSR     '*CANCL'
056400070411     C***
056500070411
056600990910
056700000613     C     *inzsr        BEGSR
056800990910     C*
056900990910     C     *entry        plist
057000990920     C                   parm                    tivlrds
057100990921     C                   parm      wrkesito      esito
057200000724     C                   parm                    prmlit
057300000710     C                   parm                    prmfir
057400000613     C*
057500000830     C* CALCOLA LA DATA CORRENTE
057600100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
057700100722     C                   eval      datcor = %dec(%date() : *ISO)
057800000830     C*
057900000613     C                   ENDSR
058000000613     C***
