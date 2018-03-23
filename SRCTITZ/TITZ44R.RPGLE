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
008500040910     C*
008600921023     C                   DO        *HIVAL
008700990913     C*
008800990915     C                   READ      tivin00r                               70
008900040910     C                   if        vindta > *blanks
009000000613     C                   add       1             rrnum
009100000801     C*
009200000801     C                   if        *in70 = *off
009300000801     C                             and
009400000801     C                             (vinflg = *blanks
009500000801     C                              or vinflg = '0'
009600000801     C                              or vinflg = '2')
009700000801     C*
009800000801     C                   clear                   vinmsg
009900000801     C                   eval      vinflg = '1'
010000040910     C*
010100040910     C* Eseguo routine d traduzione
010200040910     C                   exsr      impvabvat
010300040802     C*
010400010305     C                   endif
010500000905     C*
010600000905     C                   else
010700000905     C                   eval      vinflg = '1'
010800000905     C                   endif
010900000905     C*
011000000905     C  N70              update    tivin000
011100000905     C*
011200991022     C  N70              ENDdo
011300100722     C*
011400100722     C* Scarico i buffer testata ancora "in canna"
011500100722     C                   WRITE     FIVAB000
011600010202     C*
011700010202     C                   endif
011800990910
011900990910     C* Se non ci sono record con errori ...
012000000710     C                   if        §ctrno = 0
012100990910     C* ... restituisco esito OK.
012200990921     C                   eval      wrkesito = '0'
012300990910     C                   else
012400010201     C                   if        §ctrokvb > 0
012500990921     C                   eval      wrkesito = '1'
012600000710     C                   else
012700000710     C                   eval      wrkesito = '2'
012800990910     C                   endif
012900000710     C                   endif
013000990910     C*
013100990914     C                   if        %open(tivin00r)
013200990908     C                   close     tivin00r
013300990914     C                   endif
013400021113     C                   if        %open(fivabwwr)
013500021113     C                   close     fivabwwr
013600990914     C                   endif
013700021113     C                   if        %open(fivatwwr)
013800021113     C                   close     fivatwwr
013900010201     C                   endif
014000990910     C*
014100010201     C                   if        §ctrokvb > 0
014200000724     C                             and vlrpoi <> *zeros
014300010202     C                   exsr      invio
014400990920     C                   endif
014500990920     C*
014600910830     C                   ENDSR
014700000613     C***
014800990920
014900000801     C*----------------------------------------------------*
015000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
015100000801     C*----------------------------------------------------*
015200010201     C     INZVAR        BEGSR
015300000801     C*
015400040802     C                   Z-ADD     *zeros        Num5_0            5 0
015500040802     C                   MOVEL     '0'           FlgCAS            1
015600000801     C*
015700000801     C                   ENDSR
015800000801     C*----------------------------------------------------*
015900040910     C*  IMPOSTAZIONE CAMPI COSTANTI
016000000801     C*----------------------------------------------------*
016100000801     C     DEFCAM        BEGSR
016200000801     C*
016300021113     C                   CLEAR                   FIVAB000
016400040802     C                   CLEAR                   FIVAT000
016500020619     C* Imposto i valori di default...
016600120716     C                   Z-ADD     0934736       VABCCM
016700120716     C                   Z-ADD     0934736       VATCCM
016800120716     C                   Z-ADD     093           VABLNP
016900120716     C                   Z-ADD     093           VATLNP
017000070531     C                   Z-ADD     000           VABCTR
017100070531     C                   MOVEL     '7Q'          VABCTM
017200040823     C                   MOVEL     '1'           VABCBO
017300120716     C                   EVAL      VABTSP = 'C'
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
021500120716     C                   IF        %subst(vindta:1:1) = '1'
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
025500120720     C* ......VABRMN
025600120716     C                   EVAL      VABRMA=%trim(%subst(vindta:2:10))
025700120716     C                   EVAL      VABRMN=%dec(%trim(%subst(vindta:2:10)):10:0)
025800120716     C* ......VABRSD
025900120716     C                   EVAL      VABRSD=%trim(%subst(vindta:16:35))
026000120716     C* ......VABCAD
026100120716     C                   EVAL      VABCAD=%trim(%subst(vindta:51:5))
026200120716     C* ......VABLOD
026300120716     C                   EVAL      VABLOD=%trim(%subst(vindta:56:30))
026400120716     C* ......VABPRD
026500120716     C                   EVAL      VABPRD=%trim(%subst(vindta:86:2))
026600150115     C* ......VABTIC
026700150115     C                   EVAL      VABTIC=%trim(%subst(vindta:257:2))
026800160527     C* ......VABGMA
026900160527     C                   EVAL      VABGMA=%trim(%subst(vindta:269:2))
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
028200110207     C* ......VABNCL
028300120716     C                   EVAL      PiStr=%trim(%subst(vindta:88:6))
028400100714     C                   EXSR      CHKNUM
028500100714     C                   IF        PiInt=*on
028600110211     C                   Z-ADD     PiVal         VABNCL
028700100714     C                   ELSE
028800100714     C                   ADD       1             errore
028900100714     C                   EVAL      vinmsg = %trimr(vinmsg)
029000100714     C                             + ' ' + 'VABNCL'
029100120716     C                   ENDIF
029200060202     C* ......VABPKB
029300120716     C                   EVAL      PiStr=%trim(%subst(vindta:94:8))
029400060202     C                   EXSR      CHKNUM
029500060202     C                   IF        PiNum=*on
029600120716     C                   EVAL      PiVal = PiVal / 100                          gest. 2 dec.
029700070726     C                   Z-ADD(H)  PiVal         VABPKB
029800060202     C                   ELSE
029900060202     C                   ADD       1             errore
030000060202     C                   EVAL      vinmsg = %trimr(vinmsg)
030100060202     C                             + ' ' + 'VABPKB'
030200060202     C                   ENDIF
030300120716     C* ......VABIND
030400120716     C                   EVAL      VABIND=%trim(%subst(vindta:102:35))
030500120720     C* ......VABRMA
030600120720     C                   EVAL      VABRMA=%trim(%subst(vindta:137:12))
030700120716     C* ......VABNOT/NT2
030800120720     C                   EVAL      VABNOT=%subst(vindta:150:35)
030900120720     C                   EVAL      VABNT2=%subst(vindta:150+35:35)
031000150115     C* ..... VABCAS
031100150115     C                   IF        %subst(vindta:259:10) <> *blank
031200150115     C                             and %subst(vindta:259:10) <> '0000000000'
031300150115     C                   EVAL      vabVCA = 'EUR'
031400150115     C                   MOVEL     '1'           FlgCAS
031500150115     C                   EVAL      PiStr=
031600150115     C                              %trim(%subst(vindta:259:10))
031700150115     C                   EXSR      CHKNUM
031800150115     C                   IF        PiNum=*on
031900150115     C                   EVAL      VABCAS = PiVal/100
032000150115     C                   ELSE
032100150115     C                   SETON                                        32
032200150115     C                   EVAL      vinmsg = %trimr(vinmsg)
032300150115     C                             + ' ' + 'VABCAS'
032400150115     C                   ENDIF
032500150115     C                   ENDIF
032600060202     C*
032700060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
032800060202     C                   IF        FlgCAS <> '0'
032900060202     C                   IF        VABCBO = '1'
033000060202     C                   EVAL      VABCBO = '4'
033100060202     C                   ELSE
033200060202     C                   EVAL      VABCBO = '6'
033300060202     C                   ENDIF
033400060202     C                   ENDIF
033500060202     C*
033600060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
033700060202     C                   EXSR      CHKIMPDIV
033800040910     C*
033900040910     C                   ENDIF
034000110425     C***
034100120716     C* ...tipo record '2' (informazioni aggiuntive di consegna)
034200120716     C                   IF        %subst(vindta:1:1) = '2'
034300070719     C* Resetto indicatore d anomalia sul singolo record
034400070719     C                   eval      vinflg = '1'
034500100722     C* ......VATNOT
034600120716     C                   EVAL      VATNOT=%trim(%subst(vindta:32:13))
034700100722     C                   EVAL      VATTRC = 'E'
034800120716     C* x ogni tipo record '2' devo scaricare il buffer del file FIVAT
034900100722     C                   IF        VATNOT <> *blanks
035000100722     C                   ADD       1             §CTROKVT
035100100722     C                   WRITE     FIVAT000
035200100722     C                   ENDIF
035300100722     C*
035400100722     C                   ENDIF
035500010202     C*
035600000801     C* Ebbene...
035700000801     C                   ADD       1             §CTRMO
035800010201     C                   IF        errore <> *zeros
035900000801     C                   ADD       1             §CTRNO
036000000801     C                   EVAL      vinflg = '2'
036100000801     C                   ELSE
036200010201     C                   ADD       1             §CTROKVB
036300000801     C                   ENDIF
036400000801     C*
036500000801     C                   ENDSR
036600010202     C*----------------------------------------------------*
036700021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
036800010202     C*----------------------------------------------------*
036900020305     C     PREVAT        BEGSR
037000010202     C*
037100021113     C* Compongo il nome del membro da dare al FIVATWWR
037200010202     C                   eval      parmbr = vlrhdl
037300010202     C                   movel     'M'           parmbr
037400060113     C                   eval      parccm = vlrksc
037500010202     C                   eval      paropz = '1'
037600010202     C* Effettuo la chiamata al CLLE preposto
037700040506     C                   call(e)   'TITVVTC'
037800010202     C                   parm                    parccm
037900010202     C                   parm                    parmbr
038000010202     C                   parm                    paropz
038100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
038200010202     C                   if        %error
038300010202     C                   movel     '1'           chkcall
038400010202     C                   else
038500010202     C                   movel     '0'           chkcall
038600010202     C                   endif
038700010202     C*
038800010202     C                   ENDSR
038900000801     C*----------------------------------------------------*
039000000801     C*  CONTROLLO NUMERICITA' CAMPI
039100000801     C*----------------------------------------------------*
039200000801     C     CHKNUM        BEGSR
039300000801     C*
039400000801     C                   call(e)   'ISNUMERIC'
039500000801     C                   PARM                    PiStr            30
039600040714     C                   PARM      ','           PiDecChr          1
039700000801     C                   PARM      *ZEROS        PiVal            30 9
039800000801     C                   PARM      '0'           PiInt             1
039900000801     C                   PARM      '0'           PiNum             1
040000000801     C                   IF        %error
040100000801     C                   EVAL      PiInt=*off
040200000801     C                   ENDIF
040300000801     C*
040400000801     C                   ENDSR
040500000801     C***
040600000801
040700011113
040800011113     C*----------------------------------------------------*
040900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
041000011113     C*----------------------------------------------------*
041100011113     C     CHKIMPDIV     BEGSR
041200011113     C*
041300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
041400011113     C                   Z-ADD     *zeros        wrkDec            9 9
041500011113     C*
041600011113     C* Come prima cosa effettuo considerazioni sulla divisa
041700011113     C                   IF        vabIAS > *zeros
041800011113     C                   IF        vabVAS <> 'EUR'
041900011113     C                   EVAL      vabVAS =  'ITL'
042000011113     C                   ENDIF
042100011113     C                   ENDIF
042200011113     C*
042300011113     C                   IF        vabCAS > *zeros
042400011113     C                   IF        vabVCA <> 'EUR'
042500011113     C                   EVAL      vabVCA =  'ITL'
042600011113     C                   ENDIF
042700011113     C                   ENDIF
042800011113     C*
042900011113     C                   IF        vabVMD > *zeros
043000020305     C                   IF        vabVAD <> 'EUR'
043100011113     C                   EVAL      vabVAD =  'ITL'
043200011113     C                   ENDIF
043300011113     C                   ENDIF
043400011113     C*
043500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
043600011113     C                   Z-ADD     vabIAS        wrkDec
043700011113     C                   IF        wrkDec > *zeros
043800011113     C                   IF        vabVAS = 'ITL'
043900011113     C                   EVAL      vabIAS = *zeros
044000011113     C                   ENDIF
044100011113     C                   ENDIF
044200011113     C*
044300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
044400011113     C                   Z-ADD     vabCAS        wrkDec
044500011113     C                   IF        wrkDec > *zeros
044600011113     C                   IF        vabVCA = 'ITL'
044700011113     C                   EVAL      vabCAS = *zeros
044800011113     C                   ENDIF
044900011113     C                   ENDIF
045000011113     C*
045100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
045200011113     C                   Z-ADD     vabVMD        wrkDec
045300011113     C                   IF        wrkDec > *zeros
045400011113     C                   IF        vabVAD = 'ITL'
045500011113     C                   EVAL      vabVMD = *zeros
045600011113     C                   ENDIF
045700011113     C                   ENDIF
045800011113     C*
045900011113     C                   ENDSR
046000011113     C***
046100011113
046200011113
046300000801
046400000801
046500990920      /TITLE Invio dei dati al punto operativo.
046600010202     C     invio         BEGSR
046700990920     C*
046800021113     C* 1° invio FIVAT
046900010201     C                   reset                   dscmz
047000010201     C                   move      vlrpoi        cmzdst
047100021113     C                   eval      cmzfld = 'FIVATWWR'
047200010201     C                   eval      cmzmbd = vlrhdl
047300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
047400021009     C***                if        prmfir = *blanks
047500021113     C                   eval      cmzfla = 'FIVAT00F'
047600021113     C                   eval      cmzmba = 'FIVAT00F'
047700021009     C***                else
047800021009     C***                eval      cmzfla = prmfir
047900021009     C***                eval      cmzmba = prmfir
048000021009     C***                endif
048100010201     C                   eval      cmznrr = *zeros
048200020305     C                   move      §ctrokvt      cmznrr
048300021018     C                   eval      cmzlba = vlrfl1
048400010201     C                   call(e)   'TIS711C'
048500010201     C                   parm                    dscmz
048600010201     C                   parm      *blanks       esito
048700010205     C                   if        %error
048800010205     C                             or cmzerr = '1'
048900010205     C                             or esito  = '1'
049000010205     C                   eval      wrkesito = '3'
049100010205     C                   else
049200010201     C*
049300021113     C* 2° invio FIVAB
049400010201     C                   reset                   dscmz
049500010201     C                   move      vlrpoi        cmzdst
049600010201     C                   eval      cmzfld = vlrfou
049700010201     C                   eval      cmzmbd = vlrhdl
049800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
049900021009     C***                if        prmfir = *blanks
050000021113     C                   eval      cmzfla = 'FIVAB00F'
050100021113     C                   eval      cmzmba = 'FIVAB00F'
050200021009     C***                else
050300021009     C***                eval      cmzfla = prmfir
050400021009     C***                eval      cmzmba = prmfir
050500021009     C***                endif
050600010201     C                   eval      cmznrr = *zeros
050700010201     C                   move      §ctrokvb      cmznrr
050800021018     C                   eval      cmzlba = vlrfl1
050900010201     C                   call(e)   'TIS711C'
051000010201     C                   parm                    dscmz
051100010201     C                   parm      *blanks       esito
051200010201     C                   if        %error
051300010201     C                             or cmzerr = '1'
051400010201     C                             or esito  = '1'
051500010201     C                   eval      wrkesito = '3'
051600010201     C                   endif
051700010205     C                   endif
051800990920     C*
051900000613     C                   ENDSR
052000000613     C***
052100070411
052200070411     C     *pssr         BEGSR
052300070411     C*
052400070411     C                   if        %open(tivin00r)
052500070411     C                   close     tivin00r
052600070411     C                   endif
052700070411     C                   if        %open(fivabwwr)
052800070411     C                   close     fivabwwr
052900070411     C                   endif
053000070411     C                   if        %open(fivatwwr)
053100070411     C                   close     fivatwwr
053200070411     C                   endif
053300070411     C*
053400070411     C* Effettuo la chiamata al CLLE preposto
053500070411     C                   call(e)   'TITVVTC'
053600070411     C                   parm                    parccm
053700070411     C                   parm                    parmbr
053800070411     C                   parm      '2'           paropz
053900070411     C*
054000070411     C                   eval      wrkesito = '2'
054100070411     C*
054200070411     C                   seton                                        LR
054300070411     C*
054400070411     C                   ENDSR     '*CANCL'
054500070411     C***
054600070411
054700990910
054800000613     C     *inzsr        BEGSR
054900990910     C*
055000990910     C     *entry        plist
055100990920     C                   parm                    tivlrds
055200990921     C                   parm      wrkesito      esito
055300000724     C                   parm                    prmlit
055400000710     C                   parm                    prmfir
055500000613     C*
055600000830     C* CALCOLA LA DATA CORRENTE
055700100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
055800100722     C                   eval      datcor = %dec(%date() : *ISO)
055900000830     C*
056000000613     C                   ENDSR
056100000613     C***
