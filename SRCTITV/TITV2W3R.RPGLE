000100150813      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200130115     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300120207     H DECEDIT('0,') DATEDIT(*DMY.)
000400990908
000500990910     Ftivin00r  uF   E             DISK    usropn
000600150813     FEDIVABwr  O    E             DISK    usropn
000700150813     FEDIVATwr  O    E             DISK    usropn
000800990908
000900000801     D*----------------------------------------------------
001000000801     D* DICHIARAZIOINE VARIABILI DI WRK
001100000801     D*----------------------------------------------------
001200990920     D dscmz         e ds                  inz
001300990910     D psds           sds
001400990910     D  procname         *PROC
001500990920     D tivlrds       e ds                  extname(tivlr00f)
001600040506     D tisi95ds      e ds
001700990910     D esito           s              1
001800000724     D prmlit          s             10
001900000710     D prmfir          s             10
002000990921     D wrkesito        s                   like(esito)
002100000613     D rrnum           s              6  0 INZ(*zeros)
002200010202     D parccm          s              8    INZ(*blanks)
002300010202     D parmbr          s             10    INZ(*blanks)
002400010202     D paropz          s              1    INZ(*blanks)
002500010202     D chkcall         s              1    INZ(*blanks)
002600120130     D wOut            s              1    INZ('0')
002700120207     D wNomeFile       s             30    INZ(*blanks)
002800000830
002900041025     D*------------------
003000041025     D* DS REPERIMENTO NUMERATORE
003100041025     D*------------------
003200041025     D trul33ds      e ds                  inz
003300041025     D*------------------
003400041025     D* DS ARCHITETTURA
003500041025     D*------------------
003600041025     D kpjba         e ds                  inz
003700060913     D*------------------
003800990908
003900081222
004000081222     D*------------------
004100081222     D* LINKING A DEFINIZIONI ESTERNE
004200081222     D*------------------
004300081222     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004400081222     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004500010201
004600010201
004700000913     C                   reset                   rrnum
004800990921     C                   reset                   esito
004900990921     C                   reset                   wrkesito
005000000613     C*
005100040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005200000613     C*
005300010202     C* Effettuo la chiamata al CLLE preposto
005400150813     C                   call(e)   'TITVEVTC'
005500010202     C                   parm                    parccm
005600010202     C                   parm                    parmbr
005700010202     C                   parm      '2'           paropz
005800040929     C*
005900040929     C* Esegue lancio TISI95R solo x chiusura
006000040929     C                   CLEAR                   TISI95DS
006100040929     C                   EVAL      I95TLA = 'C'
006200040929     C                   CALL      'TISI95R'
006300040929     C                   PARM                    TISI95DS
006400000616     C*
006500010201     C                   seton                                        LR
006600990908
006700000801
006800910830     C*--------------------------------------------------------
006900150813     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
007000910830     C*--------------------------------------------------------
007100040526     C     RWFILE        BEGSR
007200990910     C*
007300990914     C                   if        not %open(tivin00r)
007400990908     C                   open      tivin00r
007500990914     C                   endif
007600150813     C                   if        not %open(edivabwr)
007700150813     C                   open      edivabwr
007800990914     C                   endif
007900150813     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
008000020305     C                   exsr      prevat
008100010201     C*
008200010202     C                   if        chkcall = '0'
008300010202     C*
008400150813     C                   if        not %open(edivatwr)
008500150813     C                   open      edivatwr
008600010201     C                   endif
008700990910     C*
008800010201     C                   clear                   §CTROKVB          5 0
008900020305     C                   clear                   §CTROKVT          5 0
009000000801     C                   clear                   §CTRMO            5 0
009100000801     C                   clear                   §CTRNO            5 0
009200040910     C*
009300921023     C                   DO        *HIVAL
009400990913     C*
009500990915     C                   READ      tivin00r                               70
009600040910     C                   if        vindta > *blanks
009700000613     C                   add       1             rrnum
009800000801     C*
009900000801     C                   if        *in70 = *off
010000000801     C                             and
010100000801     C                             (vinflg = *blanks
010200000801     C                              or vinflg = '0'
010300000801     C                              or vinflg = '2')
010400000801     C*
010500000801     C                   clear                   vinmsg
010600000801     C                   eval      vinflg = '1'
010700040910     C*
010800040910     C* Eseguo routine d traduzione
010900040910     C                   exsr      impvabvat
011000161007     C*
011100161007     C* Scarico il buffer di output
011200161007     C                   write     EDIVAB00
011300161007     C                   if        vatNOT <> *blanks
011400161007     C                   write     EDIVAT00
011500161007     C                   endif
011600040802     C*
011700010305     C                   endif
011800000905     C*
011900000905     C                   else
012000000905     C                   eval      vinflg = '1'
012100000905     C                   endif
012200000905     C*
012300000905     C  N70              update    tivin000
012400000905     C*
012500991022     C  N70              ENDdo
012600120130     C*
012700010202     C                   endif
012800990910
012900990910     C* Se non ci sono record con errori ...
013000000710     C                   if        §ctrno = 0
013100990910     C* ... restituisco esito OK.
013200990921     C                   eval      wrkesito = '0'
013300990910     C                   else
013400010201     C                   if        §ctrokvb > 0
013500990921     C                   eval      wrkesito = '1'
013600000710     C                   else
013700000710     C                   eval      wrkesito = '2'
013800990910     C                   endif
013900000710     C                   endif
014000990910     C*
014100990914     C                   if        %open(tivin00r)
014200990908     C                   close     tivin00r
014300990914     C                   endif
014400150813     C                   if        %open(edivabwr)
014500150813     C                   close     edivabwr
014600990914     C                   endif
014700150813     C                   if        %open(edivatwr)
014800150813     C                   close     edivatwr
014900010201     C                   endif
015000990910     C*
015100010201     C                   if        §ctrokvb > 0
015200000724     C                             and vlrpoi <> *zeros
015300010202     C                   exsr      invio
015400990920     C                   endif
015500990920     C*
015600910830     C                   ENDSR
015700000613     C***
015800990920
015900000801     C*----------------------------------------------------*
016000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
016100000801     C*----------------------------------------------------*
016200010201     C     INZVAR        BEGSR
016300000801     C*
016400040802     C                   Z-ADD     *zeros        Num5_0            5 0
016500040802     C                   MOVEL     '0'           FlgCAS            1
016600000801     C*
016700000801     C                   ENDSR
016800000801     C*----------------------------------------------------*
016900040910     C*  IMPOSTAZIONE CAMPI COSTANTI
017000000801     C*----------------------------------------------------*
017100000801     C     DEFCAM        BEGSR
017200000801     C*
017300150813     C                   CLEAR                   EDIVAB00
017400150813     C                   CLEAR                   EDIVAT00
017500020619     C* Imposto i valori di default...
017600161007     C                   Z-ADD     0936513       VABCCM
017700161007     C                   Z-ADD     0936513       VATCCM
017800161007     C                   Z-ADD     093           VABLNP
017900161007     C                   Z-ADD     093           VATLNP
018000161007     C                   Z-ADD     500           VABCTR
018100161007     C                   MOVEL     '7 '          VABCTM
018200040823     C                   MOVEL     '1'           VABCBO
018300161007     C                   MOVEL     'C'           VABTSP
018400020619     C* ... e poi verifico se sono stati passati come parametri
018500020619     C                   IF        vlrppt > *blanks
018600040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
018700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
018800020619     C                   EXSR      CHKNUM
018900020619     C                   IF        PiInt=*on
019000020619     C                   Z-ADD     PiVal         VABCCM
019100020619     C                   Z-ADD     PiVal         VATCCM
019200020619     C                   ENDIF
019300040506     C                   ENDIF
019400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
019500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
019600020619     C                   EXSR      CHKNUM
019700020619     C                   IF        PiInt=*on
019800020619     C                   Z-ADD     PiVal         VABLNP
019900020619     C                   Z-ADD     PiVal         VATLNP
020000040506     C                   ENDIF
020100020619     C                   ENDIF
020200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
020300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
020400020619     C                   EXSR      CHKNUM
020500020619     C                   IF        PiInt=*on
020600020619     C                   Z-ADD     PiVal         VABCTR
020700040506     C                   ENDIF
020800020619     C                   ENDIF
020900070427     C                   IF        %subst(vlrppt:14:1) <> *blanks
021000070427     C                   EVAL      VABTSP = %subst(vlrppt:14:1)
021100070427     C                   ENDIF
021200020619     C                   ENDIF
021300000801     C*
021400000801     C                   ENDSR
021500000801     C*----------------------------------------------------*
021600150813     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
021700000801     C*----------------------------------------------------*
021800040910     C     IMPVABVAT     BEGSR
021900120130     C*
022000040910     C* ......inizializzazioni iniziali e formati record file Bartolini
022100040910     C                   EXSR      INZVAR
022200040910     C                   EXSR      DEFCAM
022300081222     C*
022400081222     C                   Z-ADD     *zeros        errore            1 0
022500161007     C*
022600081222     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
022700081222     C                   MOVEL     datcor        VABAAS
022800081222     C                   MOVEL     datcor        VATAAS
022900081222     C                   MOVE      datcor        VABMGS
023000081222     C                   MOVE(P)   vlrpoi        VABFGS
023100081222     C                   MOVE(P)   vlrpoi        VATFGS
023200150813     C*
023300150813     C* ......Dati CMR
023400161007     C                   EVAL      VABCMR = 'Manifest: ' + %char(datcor)
023500151203     C***                EVAL      VABCMR = wNomeFile
023600150813     C                   EVAL      VABDCM = datcor
023700150813     C                   EVAL      VABDTS = datcor
023800150813     C                   EVAL      VABCNT = 1
023900150813     C                   EVAL      VATCMR = VABCMR
024000150813     C                   EVAL      VATCNT = VABCNT
024100161007     C*
024200041025     C* NSP => Stacco un numeratore da AZNUM
024300041025     C                   clear                   TRUL33DS
024400041025     C                   eval      I33OPE = *zeros
024500041025     C                   eval      I33CNU = 302
024600041025     C                   eval      I33NUM = 1
024700041025     C                   movel     TRUL33DS      KPJBU
024800041025     C                   call      'TRUL33R'
024900041025     C                   parm                    KPJBA
025000041025     C                   movel     KPJBU         TRUL33DS
025100041025     C                   if        O33ERR = *zeros
025200041025     C                   z-add     O33NRF        VABNSP
025300041025     C                   z-add     O33NRF        VATNSP
025400041025     C                   else
025500041025     C                   Z-ADD     1             errore
025600041025     C                   EVAL      vinmsg = %trimr(vinmsg)
025700041025     C                             + ' ' + 'VABNSP VATNSP'
025800041025     C                   endif
025900040910     C* ......VABRSD
026000161007     C                   EVAL      VABRSD=%trim(%subst(vindta:120:30))
026100060928     C* == sostituisco caratteri che bloccano POSTEL/piccione ==
026200060928     C     '@':'A'       XLATE     VABRSD        VABRSD
026300060928     C     '§':'S'       XLATE     VABRSD        VABRSD
026400060928     C     '!':' '       XLATE     VABRSD        VABRSD
026500060928     C     ':':' '       XLATE     VABRSD        VABRSD
026600040910     C* ......VABIND
026700161007     C                   EVAL      VABIND=%trim(%subst(vindta:150:30))
026800040910     C* ......VABCAD
026900161007     C                   EVAL      PiStr=%trim(%subst(vindta:180:5))
027000040910     C                   EXSR      CHKNUM
027100040910     C                   IF        PiInt=*on
027200040910     C                   Z-ADD     PiVal         Num5_0
027300040910     C                   MOVEL(P)  Num5_0        VABCAD
027400040910     C                   ELSE
027500040910     C                   ADD       1             errore
027600040910     C                   EVAL      vinmsg = %trimr(vinmsg)
027700040910     C                             + ' ' + 'VABCAD'
027800040910     C                   ENDIF
027900040910     C* ......VABLOD
028000161007     C                   EVAL      VABLOD=%trim(%subst(vindta:185:25))
028100161007     C* ......VABPRD
028200161007     C                   EVAL      VABPRD=%trim(%subst(vindta:210:2))
028300161007     C
028400040910     C* Reperisco la provincia dal CAP e dalla località
028500040910     C                   IF        VABCAD <> *blanks AND
028600161007     C                             VABPRD  = *blanks AND
028700161007     C                             VABNZD  = *blanks
028800040910     C                   CLEAR                   TISI95DS
028900040910     C                   EVAL      I95TCN = '3'
029000040910     C                   Z-ADD     datcor        I95DAT
029100040910     C                   EVAL      I95CAP = VABCAD
029200040910     C                   EVAL      I95LOC = VABLOD
029300040910     C                   CALL      'TISI95R'
029400040910     C                   PARM                    TISI95DS
029500040910     C                   EVAL      VABPRD = O95PRV
029600040910     C                   ENDIF
029700081222     C* ......VABRMA
029800161007     C                   EVAL      VABRMA=%trim(%subst(vindta:42:7))
029900120130     C* ......VABRMN
030000161007     C                   EVAL      PiStr=%trim(%subst(vindta:42:7))
030100120130     C                   EXSR      CHKNUM
030200120130     C                   IF        PiInt=*on
030300120130     C                   ADD       PiVal         VABRMN
030400120130     C                   ELSE
030500120130     C                   ADD       1             errore
030600120130     C                   EVAL      vinmsg = %trimr(vinmsg)
030700120130     C                             + ' ' + 'VABRMN'
030800120130     C                   ENDIF
030900040910     C* ......VABNCL
031000161007     C                   EVAL      PiStr=%trim(%subst(vindta:73:4))
031100040910     C                   EXSR      CHKNUM
031200040910     C                   IF        PiInt=*on
031300050719     C                   ADD       PiVal         VABNCL
031400040910     C                   ELSE
031500040910     C                   ADD       1             errore
031600040910     C                   EVAL      vinmsg = %trimr(vinmsg)
031700040910     C                             + ' ' + 'VABNCL'
031800040910     C                   ENDIF
031900040910     C* ......VABPKB
032000161007     C                   EVAL      PiStr=%trim(%subst(vindta:62:11))
032100040910     C                   EXSR      CHKNUM
032200040910     C                   IF        PiNum=*on
032300161007     C                   EVAL(H)   VABPKB=PiVal/1000                            * gestisco 3 dec.
032400040910     C                   ELSE
032500040910     C                   ADD       1             errore
032600040910     C                   EVAL      vinmsg = %trimr(vinmsg)
032700040910     C                             + ' ' + 'VABPKB'
032800040910     C                   ENDIF
032900161007     C* ......VABVLB / VABQFT
033000161007     C                   EVAL      PiStr=%trim(%subst(vindta:110:10))
033100161007     C                   EXSR      CHKNUM
033200161007     C                   IF        PiNum=*on
033300161007     C                   EVAL(H)   VABVLB=PiVal/100000                          * gestisco 5 dec.
033400161007     C                   EVAL(H)   VABQFT=PiVal/100000                          * gestisco 5 dec.
033500161007     C                   ELSE
033600161007     C                   ADD       1             errore
033700161007     C                   EVAL      vinmsg = %trimr(vinmsg)
033800161007     C                             + ' ' + 'VABVLB/VABQFT'
033900161007     C                   ENDIF
034000040910     C* ......VABCAS
034100161007     C                   IF        %subst(vindta:77:3) = '004' OR
034200161007     C                             %subst(vindta:77:3) = '033' OR
034300161007     C                             %subst(vindta:77:3) = '040' OR
034400161007     C                             %subst(vindta:77:3) = '056' OR
034500161007     C                             %subst(vindta:77:3) = '070' OR
034600161007     C                             %subst(vindta:77:3) = '075'
034700040910     C                   EVAL      FlgCAS = '1'
034800161007     C                   EVAL      VABTIC = 'BM'
034900161007     C                   EVAL      VABVCA = 'EUR'
035000161007     C                   EVAL      PiStr=%trim(%subst(vindta:49:13))
035100040910     C                   EXSR      CHKNUM
035200040910     C                   IF        PiNum=*on
035300040910     C                   EVAL(H)   VABCAS=PiVal/100                             * gestisco 2 dec.
035400040910     C                   ELSE
035500040910     C                   ADD       1             errore
035600040910     C                   EVAL      vinmsg = %trimr(vinmsg)
035700040910     C                             + ' ' + 'VABCAS'
035800040910     C                   ENDIF
035900161007     C                   ENDIF
036000161007     C*
036100161007     C* Considerazioni finali su CBO/CAS
036200161007     C                   IF        FlgCAS = '1'    OR
036300161007     C                             VABCAS  > *zeros
036400161007     C                   IF        VABCBO = '1'
036500161007     C                   EVAL      VABCBO = '4'
036600161007     C                   ENDIF
036700161007     C                   IF        VABCBO = '2'
036800161007     C                   EVAL      VABCBO = '6'
036900161007     C                   ENDIF
037000161007     C                   ENDIF
037100010202     C*
037200000801     C* Ebbene...
037300000801     C                   ADD       1             §CTRMO
037400010201     C                   IF        errore <> *zeros
037500000801     C                   ADD       1             §CTRNO
037600000801     C                   EVAL      vinflg = '2'
037700000801     C                   ELSE
037800010201     C                   ADD       1             §CTROKVB
037900000801     C                   ENDIF
038000000801     C*
038100000801     C                   ENDSR
038200010202     C*----------------------------------------------------*
038300150813     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
038400010202     C*----------------------------------------------------*
038500020305     C     PREVAT        BEGSR
038600010202     C*
038700150813     C* Compongo il nome del membro da dare al EDIVATWR
038800010202     C                   eval      parmbr = vlrhdl
038900010202     C                   movel     'M'           parmbr
039000060113     C                   eval      parccm = vlrksc
039100010202     C                   eval      paropz = '1'
039200010202     C* Effettuo la chiamata al CLLE preposto
039300150813     C                   call(e)   'TITVEVTC'
039400010202     C                   parm                    parccm
039500010202     C                   parm                    parmbr
039600010202     C                   parm                    paropz
039700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
039800010202     C                   if        %error
039900010202     C                   movel     '1'           chkcall
040000010202     C                   else
040100010202     C                   movel     '0'           chkcall
040200010202     C                   endif
040300010202     C*
040400010202     C                   ENDSR
040500000801     C*----------------------------------------------------*
040600000801     C*  CONTROLLO NUMERICITA' CAMPI
040700000801     C*----------------------------------------------------*
040800000801     C     CHKNUM        BEGSR
040900081222     C*
041000081222     C                   IF        PiDecChr = *blanks
041100081222     C                   EVAL      PiDecChr = ','
041200081222     C                   ENDIF
041300081222     C*
041400081222     C                   callp(e)  UBISNUM_Check(PiStr
041500081222     C                                          :PiDecChr
041600081222     C                                          :PiVal
041700081222     C                                          :PiNum
041800081222     C                                          :PiInt)
041900081222     C*
042000081222     C                   IF        %error
042100081222     C                   EVAL      PiInt=*off
042200081222     C                   ENDIF
042300000801     C*
042400000801     C                   ENDSR
042500000801     C***
042600000801
042700011113
042800011113     C*----------------------------------------------------*
042900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
043000011113     C*----------------------------------------------------*
043100011113     C     CHKIMPDIV     BEGSR
043200011113     C*
043300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
043400011113     C                   Z-ADD     *zeros        wrkDec            9 9
043500011113     C*
043600011113     C* Come prima cosa effettuo considerazioni sulla divisa
043700011113     C                   IF        vabIAS > *zeros
043800011113     C                   IF        vabVAS <> 'EUR'
043900011113     C                   EVAL      vabVAS =  'ITL'
044000011113     C                   ENDIF
044100011113     C                   ENDIF
044200011113     C*
044300011113     C                   IF        vabCAS > *zeros
044400011113     C                   IF        vabVCA <> 'EUR'
044500011113     C                   EVAL      vabVCA =  'ITL'
044600011113     C                   ENDIF
044700011113     C                   ENDIF
044800011113     C*
044900011113     C                   IF        vabVMD > *zeros
045000020305     C                   IF        vabVAD <> 'EUR'
045100011113     C                   EVAL      vabVAD =  'ITL'
045200011113     C                   ENDIF
045300011113     C                   ENDIF
045400011113     C*
045500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
045600011113     C                   Z-ADD     vabIAS        wrkDec
045700011113     C                   IF        wrkDec > *zeros
045800011113     C                   IF        vabVAS = 'ITL'
045900011113     C                   EVAL      vabIAS = *zeros
046000011113     C                   ENDIF
046100011113     C                   ENDIF
046200011113     C*
046300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
046400011113     C                   Z-ADD     vabCAS        wrkDec
046500011113     C                   IF        wrkDec > *zeros
046600011113     C                   IF        vabVCA = 'ITL'
046700011113     C                   EVAL      vabCAS = *zeros
046800011113     C                   ENDIF
046900011113     C                   ENDIF
047000011113     C*
047100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
047200011113     C                   Z-ADD     vabVMD        wrkDec
047300011113     C                   IF        wrkDec > *zeros
047400011113     C                   IF        vabVAD = 'ITL'
047500011113     C                   EVAL      vabVMD = *zeros
047600011113     C                   ENDIF
047700011113     C                   ENDIF
047800011113     C*
047900011113     C                   ENDSR
048000011113     C***
048100011113
048200011113
048300000801
048400000801
048500990920      /TITLE Invio dei dati al punto operativo.
048600010202     C     invio         BEGSR
048700990920     C*
048800150813     C* 1° invio EDIVAT
048900010201     C                   reset                   dscmz
049000010201     C                   move      vlrpoi        cmzdst
049100150813     C                   eval      cmzfld = 'EDIVATWR'
049200010201     C                   eval      cmzmbd = vlrhdl
049300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
049400021009     C***                if        prmfir = *blanks
049500150813     C                   eval      cmzfla = 'EDIVAT0F'
049600150813     C                   eval      cmzmba = 'EDIVAT0F'
049700021009     C***                else
049800021009     C***                eval      cmzfla = prmfir
049900021009     C***                eval      cmzmba = prmfir
050000021009     C***                endif
050100010201     C                   eval      cmznrr = *zeros
050200020305     C                   move      §ctrokvt      cmznrr
050300021018     C                   eval      cmzlba = vlrfl1
050400010201     C                   call(e)   'TIS711C'
050500010201     C                   parm                    dscmz
050600010201     C                   parm      *blanks       esito
050700010205     C                   if        %error
050800010205     C                             or cmzerr = '1'
050900010205     C                             or esito  = '1'
051000010205     C                   eval      wrkesito = '3'
051100010205     C                   else
051200010201     C*
051300150813     C* 2° invio EDIVAB
051400010201     C                   reset                   dscmz
051500010201     C                   move      vlrpoi        cmzdst
051600010201     C                   eval      cmzfld = vlrfou
051700010201     C                   eval      cmzmbd = vlrhdl
051800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
051900021009     C***                if        prmfir = *blanks
052000150813     C                   eval      cmzfla = 'EDIVAB0F'
052100150813     C                   eval      cmzmba = 'EDIVAB0F'
052200021009     C***                else
052300021009     C***                eval      cmzfla = prmfir
052400021009     C***                eval      cmzmba = prmfir
052500021009     C***                endif
052600010201     C                   eval      cmznrr = *zeros
052700010201     C                   move      §ctrokvb      cmznrr
052800021018     C                   eval      cmzlba = vlrfl1
052900010201     C                   call(e)   'TIS711C'
053000010201     C                   parm                    dscmz
053100010201     C                   parm      *blanks       esito
053200010201     C                   if        %error
053300010201     C                             or cmzerr = '1'
053400010201     C                             or esito  = '1'
053500010201     C                   eval      wrkesito = '3'
053600010201     C                   endif
053700010205     C                   endif
053800990920     C*
053900000613     C                   ENDSR
054000000613     C***
054100070411
054200161007
054300161007
054400071219     C     *pssr         BEGSR
054500070411     C*
054600071219     C                   if        %open(tivin00r)
054700071219     C                   close     tivin00r
054800071219     C                   endif
054900150813     C                   if        %open(edivabwr)
055000150813     C                   close     edivabwr
055100071219     C                   endif
055200150813     C                   if        %open(edivatwr)
055300150813     C                   close     edivatwr
055400071219     C                   endif
055500070411     C*
055600070411     C* Effettuo la chiamata al CLLE preposto
055700150813     C                   call(e)   'TITVEVTC'
055800071219     C                   parm                    parccm
055900071219     C                   parm                    parmbr
056000071219     C                   parm      '2'           paropz
056100071219     C*
056200071219     C                   eval      wrkesito = '2'
056300071219     C*
056400071219     C                   seton                                        LR
056500070411     C*
056600071219     C                   ENDSR     '*CANCL'
056700070411     C***
056800120207
056900070411
057000990910
057100000613     C     *inzsr        BEGSR
057200990910     C*
057300990910     C     *entry        plist
057400990920     C                   parm                    tivlrds
057500990921     C                   parm      wrkesito      esito
057600000724     C                   parm                    prmlit
057700000710     C                   parm                    prmfir
057800120207     C*
057900120207     C                   MOVE(P)   vlrMSG        wNomeFile
058000000613     C*
058100000830     C* CALCOLA LA DATA CORRENTE
058200120207     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
058300120207     C                   eval      datcor = %dec(%date() : *iso)
058400000830     C*
058500000613     C                   ENDSR
058600000613     C***
