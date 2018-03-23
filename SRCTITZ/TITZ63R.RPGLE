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
015700130927     C                   MOVEL     *blanks       wNOT_B           35
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
016800130103     C                   Z-ADD     2221424       VABCCM
016900130103     C                   Z-ADD     2221424       VATCCM
017000130103     C                   Z-ADD     222           VABLNP
017100130103     C                   Z-ADD     222           VATLNP
017200070531     C                   Z-ADD     000           VABCTR
017300130103     C                   MOVEL     '7Q'          VABCTM
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
021500120716     C* ...tipo record '1' (testata documento)
021600130927     C*                     più alcuni dati di dettaglio (telefono)
021700130103     C                   IF        %subst(vindta:1:1) = 'S'
021800100722     C*
021900100722     C* Se nn primo giro => scarico il buffer precedente
022000100722     C                   if        wGiro = 1
022100100722     C                   eval      wGiro = 2
022200100722     C                   else
022300100722     C                   WRITE     FIVAB000
022400100722     C                   endif
022500100722     C*
022600071210     C* Resetto indicatore d anomalia sul singolo record
022700071210     C                   eval      vinflg = '1'
022800071210     C* ......inizializzazioni iniziali e formati record file Bartolini
022900071210     C                   EXSR      INZVAR
023000071210     C                   EXSR      DEFCAM
023100071210     C*
023200071210     C                   Z-ADD     *zeros        errore            1 0
023300071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
023400071210     C                   MOVEL     datcor        VABAAS
023500071210     C                   MOVEL     datcor        VATAAS
023600071210     C                   MOVE      datcor        VABMGS
023700071210     C                   MOVE(P)   vlrpoi        VABFGS
023800071210     C                   MOVE(P)   vlrpoi        VATFGS
023900071210     C* ......VABNSP/VATNSP
024000071210     C* NSP => Stacco un numeratore da AZNUM
024100071210     C                   clear                   TRUL33DS
024200071210     C                   eval      I33OPE = *zeros
024300071210     C                   eval      I33CNU = 302
024400071210     C                   eval      I33NUM = 1
024500071210     C                   movel     TRUL33DS      KPJBU
024600071210     C                   call      'TRUL33R'
024700071210     C                   parm                    KPJBA
024800071210     C                   movel     KPJBU         TRUL33DS
024900071210     C                   if        O33ERR = *zeros
025000071210     C                   z-add     O33NRF        VABNSP
025100071210     C                   z-add     O33NRF        VATNSP
025200071210     C                   else
025300071210     C                   Z-ADD     1             errore
025400071210     C                   EVAL      vinmsg = %trimr(vinmsg)
025500071210     C                             + ' ' + 'VABNSP VATNSP'
025600071210     C                   endif
025700130103     C* ......VABRMN / VABRMA
025800130103     C                   EVAL      VABRMN=%dec(%trim(%subst(vindta:2:15)):15:0)
025900130103     C                   EVAL      VABRMA=%trim(%subst(vindta:2:15))
026000120716     C* ......VABRSD
026100130103     C                   EVAL      VABRSD=%trim(%subst(vindta:161:30))
026200130103     C* ......VABRD2
026300130103     C                   EVAL      VABRD2=%trim(%subst(vindta:191:30))
026400130103     C* ......VABIND
026500130103     C                   EVAL      VABIND=%trim(%subst(vindta:221:30))
026600130103     C* ......VABCAD
026700130103     C                   EVAL      VABCAD=%trim(%subst(vindta:254:10))
026800120716     C* ......VABLOD
026900130103     C                   EVAL      VABLOD=%trim(%subst(vindta:264:30))
027000120716     C* ......VABPRD
027100130103     C                   EVAL      VABPRD=*blank
027200120716     C* Reperisco la provincia dal CAP e dalla località
027300120716     C                   IF        VABPRD  = *blanks AND
027400120716     C                             VABCAD <> *blanks
027500120716     C                   CLEAR                   TISI95DS
027600120716     C                   EVAL      I95TCN = '3'
027700120716     C                   Z-ADD     datcor        I95DAT
027800120716     C                   EVAL      I95CAP = VABCAD
027900120716     C                   EVAL      I95LOC = VABLOD
028000120716     C                   CALL      'TISI95R'
028100120716     C                   PARM                    TISI95DS
028200120716     C                   EVAL      VABPRD = O95PRV
028300120716     C                   ENDIF
028400130104     C* ......VABNOT/NT2
028500130927     C* la nota principale si prende da una posizione, mentre la secondaria da un altra
028600130927     C                   EVAL      VABNOT=%trim(%subst(vindta:862+16:35))
028700130927     C                   EVAL      w70=%trim(%subst(vindta:789:70))
028800130927     C                   EVAL      VABNT2=%subst(w70:01:35)
028900130927     C* ......VATNOT_B
029000130927     C                   EVAL      wNOT_B=%trim(%subst(vindta:862:16))
029100130927     C*
029200130927     C                   IF        wNOT_B <> *blanks
029300130927     C                   EVAL      VATTRC = 'B'
029400130927     C                   EVAL      VATNOT = wNOT_B
029500130927     C                   WRITE     FIVAT000
029600130927     C                   ENDIF
029700130104     C* ......VABFFD
029800130104     C                   IF        %subst(vindta:786:3) = '313'
029900130104     C                   EVAL      VABFFD='S'
030000130104     C                   ENDIF
030100130104     C                   IF        %subst(vindta:859:3) = '313'
030200130104     C                   EVAL      VABFFD='S'
030300130104     C                   ENDIF
030400130104     C                   IF        %subst(vindta:932:3) = '313'
030500130104     C                   EVAL      VABFFD='S'
030600130104     C                   ENDIF
030700130104     C* ......VABTC1
030800130927     C***                IF        %subst(vindta:786:3) = '301'
030900130927     C***                EVAL      VABTC1='A'
031000130927     C***                ENDIF
031100130104     C                   IF        %subst(vindta:859:3) = '301'
031200130104     C                   EVAL      VABTC1='A'
031300130104     C                   ENDIF
031400130927     C***                IF        %subst(vindta:932:3) = '301'
031500130927     C***                EVAL      VABTC1='A'
031600130927     C***                ENDIF
031700130927     C* ......VABTIC
031800130927     C                   IF        %subst(vindta:304:1) = 'A'
031900130927     C                   EVAL      VABTIC=*blank
032000130927     C                   ENDIF
032100130927     C                   IF        %subst(vindta:304:1) = 'B'
032200130927     C                   EVAL      VABTIC='BM'
032300130927     C                   ENDIF
032400130927     C                   IF        %subst(vindta:304:1) = 'C'
032500130927     C                   EVAL      VABTIC='CM'
032600130927     C                   ENDIF
032700110207     C* ......VABNCL
032800130103     C                   EVAL      PiStr=%trim(%subst(vindta:577:4))
032900100714     C                   EXSR      CHKNUM
033000100714     C                   IF        PiInt=*on
033100110211     C                   Z-ADD     PiVal         VABNCL
033200100714     C                   ELSE
033300100714     C                   ADD       1             errore
033400100714     C                   EVAL      vinmsg = %trimr(vinmsg)
033500100714     C                             + ' ' + 'VABNCL'
033600120716     C                   ENDIF
033700060202     C* ......VABPKB
033800130103     C                   EVAL      PiStr=%trim(%subst(vindta:549:5))
033900060202     C                   EXSR      CHKNUM
034000060202     C                   IF        PiNum=*on
034100130116     C***                EVAL      PiVal = PiVal / 10                           gest. 1 dec.
034200070726     C                   Z-ADD(H)  PiVal         VABPKB
034300060202     C                   ELSE
034400060202     C                   ADD       1             errore
034500060202     C                   EVAL      vinmsg = %trimr(vinmsg)
034600060202     C                             + ' ' + 'VABPKB'
034700060202     C                   ENDIF
034800130103     C* ......VABCAS
034900130103     C                   EVAL      VABVCA = *blank
035000130103     C                   IF        %trim(%subst(vindta:445:8))<>*blanks
035100130103     C                             AND
035200130103     C                             %trim(%subst(vindta:445:8))<>*zeros
035300130103     C                   MOVEL     '1'           FlgCAS
035400130103     C                   EVAL      PiStr=%trim(%subst(vindta:445:8))
035500130103     C                   EXSR      CHKNUM
035600130103     C                   IF        PiNum=*on
035700130103     C                   EVAL      PiVal = PiVal / 100                          gest. 2 dec.
035800130103     C                   Z-ADD     PiVal         VABCAS
035900130103     C                   EVAL      VABVCA = 'EUR'
036000130103     C                   ELSE
036100130103     C                   SETON                                        32
036200130103     C                   EVAL      vinmsg = %trimr(vinmsg)
036300130103     C                             + ' ' + 'VABCAS'
036400130103     C                   ENDIF
036500130103     C                   ENDIF
036600060202     C*
036700060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
036800060202     C                   IF        FlgCAS <> '0'
036900060202     C                   IF        VABCBO = '1'
037000060202     C                   EVAL      VABCBO = '4'
037100060202     C                   ELSE
037200060202     C                   EVAL      VABCBO = '6'
037300060202     C                   ENDIF
037400060202     C                   ENDIF
037500060202     C*
037600060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
037700060202     C                   EXSR      CHKIMPDIV
037800040910     C*
037900040910     C                   ENDIF
038000110425     C***
038100130103     C* ...tipo record 'P' (informazioni sui barcode)
038200130103     C                   IF        %subst(vindta:1:1) = 'P'
038300070719     C* Resetto indicatore d anomalia sul singolo record
038400070719     C                   eval      vinflg = '1'
038500100722     C* ......VATNOT
038600130103     C                   EVAL      VATNOT=%trim(%subst(vindta:693:20))
038700100722     C                   EVAL      VATTRC = 'E'
038800120716     C* x ogni tipo record '2' devo scaricare il buffer del file FIVAT
038900100722     C                   IF        VATNOT <> *blanks
039000100722     C                   ADD       1             §CTROKVT
039100100722     C                   WRITE     FIVAT000
039200100722     C                   ENDIF
039300100722     C*
039400100722     C                   ENDIF
039500010202     C*
039600000801     C* Ebbene...
039700000801     C                   ADD       1             §CTRMO
039800010201     C                   IF        errore <> *zeros
039900000801     C                   ADD       1             §CTRNO
040000000801     C                   EVAL      vinflg = '2'
040100000801     C                   ELSE
040200010201     C                   ADD       1             §CTROKVB
040300000801     C                   ENDIF
040400000801     C*
040500000801     C                   ENDSR
040600010202     C*----------------------------------------------------*
040700021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
040800010202     C*----------------------------------------------------*
040900020305     C     PREVAT        BEGSR
041000010202     C*
041100021113     C* Compongo il nome del membro da dare al FIVATWWR
041200010202     C                   eval      parmbr = vlrhdl
041300010202     C                   movel     'M'           parmbr
041400060113     C                   eval      parccm = vlrksc
041500010202     C                   eval      paropz = '1'
041600010202     C* Effettuo la chiamata al CLLE preposto
041700040506     C                   call(e)   'TITVVTC'
041800010202     C                   parm                    parccm
041900010202     C                   parm                    parmbr
042000010202     C                   parm                    paropz
042100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
042200010202     C                   if        %error
042300010202     C                   movel     '1'           chkcall
042400010202     C                   else
042500010202     C                   movel     '0'           chkcall
042600010202     C                   endif
042700010202     C*
042800010202     C                   ENDSR
042900000801     C*----------------------------------------------------*
043000000801     C*  CONTROLLO NUMERICITA' CAMPI
043100000801     C*----------------------------------------------------*
043200000801     C     CHKNUM        BEGSR
043300000801     C*
043400000801     C                   call(e)   'ISNUMERIC'
043500000801     C                   PARM                    PiStr            30
043600040714     C                   PARM      ','           PiDecChr          1
043700000801     C                   PARM      *ZEROS        PiVal            30 9
043800000801     C                   PARM      '0'           PiInt             1
043900000801     C                   PARM      '0'           PiNum             1
044000000801     C                   IF        %error
044100000801     C                   EVAL      PiInt=*off
044200000801     C                   ENDIF
044300000801     C*
044400000801     C                   ENDSR
044500000801     C***
044600000801
044700011113
044800011113     C*----------------------------------------------------*
044900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
045000011113     C*----------------------------------------------------*
045100011113     C     CHKIMPDIV     BEGSR
045200011113     C*
045300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
045400011113     C                   Z-ADD     *zeros        wrkDec            9 9
045500011113     C*
045600011113     C* Come prima cosa effettuo considerazioni sulla divisa
045700011113     C                   IF        vabIAS > *zeros
045800011113     C                   IF        vabVAS <> 'EUR'
045900011113     C                   EVAL      vabVAS =  'ITL'
046000011113     C                   ENDIF
046100011113     C                   ENDIF
046200011113     C*
046300011113     C                   IF        vabCAS > *zeros
046400011113     C                   IF        vabVCA <> 'EUR'
046500011113     C                   EVAL      vabVCA =  'ITL'
046600011113     C                   ENDIF
046700011113     C                   ENDIF
046800011113     C*
046900011113     C                   IF        vabVMD > *zeros
047000020305     C                   IF        vabVAD <> 'EUR'
047100011113     C                   EVAL      vabVAD =  'ITL'
047200011113     C                   ENDIF
047300011113     C                   ENDIF
047400011113     C*
047500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
047600011113     C                   Z-ADD     vabIAS        wrkDec
047700011113     C                   IF        wrkDec > *zeros
047800011113     C                   IF        vabVAS = 'ITL'
047900011113     C                   EVAL      vabIAS = *zeros
048000011113     C                   ENDIF
048100011113     C                   ENDIF
048200011113     C*
048300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
048400011113     C                   Z-ADD     vabCAS        wrkDec
048500011113     C                   IF        wrkDec > *zeros
048600011113     C                   IF        vabVCA = 'ITL'
048700011113     C                   EVAL      vabCAS = *zeros
048800011113     C                   ENDIF
048900011113     C                   ENDIF
049000011113     C*
049100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
049200011113     C                   Z-ADD     vabVMD        wrkDec
049300011113     C                   IF        wrkDec > *zeros
049400011113     C                   IF        vabVAD = 'ITL'
049500011113     C                   EVAL      vabVMD = *zeros
049600011113     C                   ENDIF
049700011113     C                   ENDIF
049800011113     C*
049900011113     C                   ENDSR
050000011113     C***
050100011113
050200011113
050300000801
050400000801
050500990920      /TITLE Invio dei dati al punto operativo.
050600010202     C     invio         BEGSR
050700990920     C*
050800021113     C* 1° invio FIVAT
050900010201     C                   reset                   dscmz
051000010201     C                   move      vlrpoi        cmzdst
051100021113     C                   eval      cmzfld = 'FIVATWWR'
051200010201     C                   eval      cmzmbd = vlrhdl
051300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
051400021009     C***                if        prmfir = *blanks
051500021113     C                   eval      cmzfla = 'FIVAT00F'
051600021113     C                   eval      cmzmba = 'FIVAT00F'
051700021009     C***                else
051800021009     C***                eval      cmzfla = prmfir
051900021009     C***                eval      cmzmba = prmfir
052000021009     C***                endif
052100010201     C                   eval      cmznrr = *zeros
052200020305     C                   move      §ctrokvt      cmznrr
052300021018     C                   eval      cmzlba = vlrfl1
052400010201     C                   call(e)   'TIS711C'
052500010201     C                   parm                    dscmz
052600010201     C                   parm      *blanks       esito
052700010205     C                   if        %error
052800010205     C                             or cmzerr = '1'
052900010205     C                             or esito  = '1'
053000010205     C                   eval      wrkesito = '3'
053100010205     C                   else
053200010201     C*
053300021113     C* 2° invio FIVAB
053400010201     C                   reset                   dscmz
053500010201     C                   move      vlrpoi        cmzdst
053600010201     C                   eval      cmzfld = vlrfou
053700010201     C                   eval      cmzmbd = vlrhdl
053800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
053900021009     C***                if        prmfir = *blanks
054000021113     C                   eval      cmzfla = 'FIVAB00F'
054100021113     C                   eval      cmzmba = 'FIVAB00F'
054200021009     C***                else
054300021009     C***                eval      cmzfla = prmfir
054400021009     C***                eval      cmzmba = prmfir
054500021009     C***                endif
054600010201     C                   eval      cmznrr = *zeros
054700010201     C                   move      §ctrokvb      cmznrr
054800021018     C                   eval      cmzlba = vlrfl1
054900010201     C                   call(e)   'TIS711C'
055000010201     C                   parm                    dscmz
055100010201     C                   parm      *blanks       esito
055200010201     C                   if        %error
055300010201     C                             or cmzerr = '1'
055400010201     C                             or esito  = '1'
055500010201     C                   eval      wrkesito = '3'
055600010201     C                   endif
055700010205     C                   endif
055800990920     C*
055900000613     C                   ENDSR
056000000613     C***
056100070411
056200070411     C     *pssr         BEGSR
056300070411     C*
056400070411     C                   if        %open(tivin00r)
056500070411     C                   close     tivin00r
056600070411     C                   endif
056700070411     C                   if        %open(fivabwwr)
056800070411     C                   close     fivabwwr
056900070411     C                   endif
057000070411     C                   if        %open(fivatwwr)
057100070411     C                   close     fivatwwr
057200070411     C                   endif
057300070411     C*
057400070411     C* Effettuo la chiamata al CLLE preposto
057500070411     C                   call(e)   'TITVVTC'
057600070411     C                   parm                    parccm
057700070411     C                   parm                    parmbr
057800070411     C                   parm      '2'           paropz
057900070411     C*
058000070411     C                   eval      wrkesito = '2'
058100070411     C*
058200070411     C                   seton                                        LR
058300070411     C*
058400070411     C                   ENDSR     '*CANCL'
058500070411     C***
058600070411
058700990910
058800000613     C     *inzsr        BEGSR
058900990910     C*
059000990910     C     *entry        plist
059100990920     C                   parm                    tivlrds
059200990921     C                   parm      wrkesito      esito
059300000724     C                   parm                    prmlit
059400000710     C                   parm                    prmfir
059500000613     C*
059600000830     C* CALCOLA LA DATA CORRENTE
059700100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
059800100722     C                   eval      datcor = %dec(%date() : *ISO)
059900000830     C*
060000000613     C                   ENDSR
060100000613     C***
