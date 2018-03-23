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
001500060529     D tisi95ds      e ds
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100010202     D parccm          s              8    INZ(*blanks)
002200010202     D parmbr          s             10    INZ(*blanks)
002300010202     D paropz          s              1    INZ(*blanks)
002400010202     D chkcall         s              1    INZ(*blanks)
002500060529     D depspe          s              9    INZ(*blanks)
002600060529     D curspe          s              9    INZ(*blanks)
002700060529     D cntspe          s              7  0 INZ(*zeros)
002800000830
002900000830     D*------------------
003000000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003100000830     D*------------------
003200000830     D WLBDA8          DS                  INZ
003300000830     D  G08DAT                 1      8  0
003400000830     D  G08INV                 9     16  0
003500000830     D  G08ERR                17     17
003600000830     D  G08TGI                18     22  0
003700000830     D*
003800990908
003900060307     D*------------------
004000060307     D* DS REPERIMENTO NUMERATORE
004100060307     D*------------------
004200060529     D trul33ds      e ds                  inz
004300060307     D*------------------
004400060307     D* DS ARCHITETTURA
004500060307     D*------------------
004600060307     D kpjba         e ds                  inz
004700060307     D*------------------
004800010201
004900010201
005000000913     C                   reset                   rrnum
005100990921     C                   reset                   esito
005200990921     C                   reset                   wrkesito
005300000613     C*
005400040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005500000613     C*
005600010202     C* Effettuo la chiamata al CLLE preposto
005700040506     C                   call(e)   'TITVVTC'
005800010202     C                   parm                    parccm
005900010202     C                   parm                    parmbr
006000010202     C                   parm      '2'           paropz
006100050201     C*
006200050201     C* Effettuo lancio TISI95 solo x chiusura
006300050201     C                   CLEAR                   TISI95DS
006400050201     C                   EVAL      I95TLA = 'C'
006500050201     C                   CALL      'TISI95R'
006600050201     C                   PARM                    TISI95DS
006700000616     C*
006800000801     C
006900010201     C                   seton                                        LR
007000990908
007100000801
007200910830     C*--------------------------------------------------------
007300040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
007400910830     C*--------------------------------------------------------
007500040526     C     RWFILE        BEGSR
007600990910     C*
007700990914     C                   if        not %open(tivin00r)
007800990908     C                   open      tivin00r
007900990914     C                   endif
008000021113     C                   if        not %open(fivabwwr)
008100021113     C                   open      fivabwwr
008200990914     C                   endif
008300021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008400020305     C                   exsr      prevat
008500010201     C*
008600010202     C                   if        chkcall = '0'
008700010202     C*
008800021113     C                   if        not %open(fivatwwr)
008900021113     C                   open      fivatwwr
009000010201     C                   endif
009100990910     C*
009200010201     C                   clear                   §CTROKVB          5 0
009300020305     C                   clear                   §CTROKVT          5 0
009400000801     C                   clear                   §CTRMO            5 0
009500000801     C                   clear                   §CTRNO            5 0
009600990910     C*
009700921023     C                   DO        *HIVAL
009800990913     C*
009900990915     C                   READ      tivin00r                               70
010000050627     C                   if        vindta > *blanks
010100000613     C                   add       1             rrnum
010200000801     C*
010300000801     C                   if        *in70 = *off
010400000801     C                             and
010500000801     C                             (vinflg = *blanks
010600000801     C                              or vinflg = '0'
010700000801     C                              or vinflg = '2')
010800000801     C*
010900000801     C                   clear                   vinmsg
011000000801     C                   eval      vinflg = '1'
011100010305     C*
011200050628     C* Determino il numero di Spedizione e a rottura eseguo operazioni
011300060529     C                   eval      PiStr=%trim(%subst(vindta:8:9))
011400050628     C                   movel(p)  PiStr         curspe
011500050628     C                   add       1             cntspe
011600050628     C*
011700050628     C                   if        cntspe = 1
011800050628     C* A rottura inizializzo campi di totalizzazione
011900050628     C                   clear                   fivab000
012000050628     C*
012100050628     C                   exsr      impvab                                       => carico VAB
012200050628     C                   eval      depspe = curspe
012300050628     C                   else
012400050628     C                   if        curspe <> depspe
012500040802     C                   exsr      wrivab
012600050628     C* A rottura inizializzo campi di totalizzazione
012700050628     C                   clear                   fivab000
012800050628     C*
012900050628     C                   exsr      impvab                                       => carico VAB
013000050628     C                   eval      depspe = curspe
013100050628     C                   else
013200050628     C                   exsr      impvab                                       => carico VAB
013300050628     C                   endif
013400050628     C                   endif
013500000905     C*
013600000905     C                   else
013700000905     C                   eval      vinflg = '1'
013800050628     C                   endif
013900000905     C                   endif
014000000905     C*
014100000905     C  N70              update    tivin000
014200000905     C*
014300991022     C  N70              ENDdo
014400040823     C*
014500040823     C* Al termine della lettura del file scarico il buffer del "VAB" rimasto "in canna"
014600040823     C                   exsr      wrivab
014700010202     C*
014800010202     C                   endif
014900990910
015000990910     C* Se non ci sono record con errori ...
015100000710     C                   if        §ctrno = 0
015200990910     C* ... restituisco esito OK.
015300990921     C                   eval      wrkesito = '0'
015400990910     C                   else
015500010201     C                   if        §ctrokvb > 0
015600990921     C                   eval      wrkesito = '1'
015700000710     C                   else
015800000710     C                   eval      wrkesito = '2'
015900990910     C                   endif
016000000710     C                   endif
016100990910     C*
016200990914     C                   if        %open(tivin00r)
016300990908     C                   close     tivin00r
016400990914     C                   endif
016500021113     C                   if        %open(fivabwwr)
016600021113     C                   close     fivabwwr
016700990914     C                   endif
016800021113     C                   if        %open(fivatwwr)
016900021113     C                   close     fivatwwr
017000010201     C                   endif
017100990910     C*
017200010201     C                   if        §ctrokvb > 0
017300000724     C                             and vlrpoi <> *zeros
017400010202     C                   exsr      invio
017500990920     C                   endif
017600990920     C*
017700910830     C                   ENDSR
017800000613     C***
017900010305
018000010305     C*----------------------------------------------------*
018100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
018200010305     C*----------------------------------------------------*
018300020305     C     WRIVAB        BEGSR
018400010305     C*
018500060225     C* Quindi scarico il buffer del file d testata
018600021113     C                   write     fivab000                                     => scarico il VAB
018700010305     C*
018800010305     C                   ENDSR
018900990920
019000000801     C*----------------------------------------------------*
019100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019200000801     C*----------------------------------------------------*
019300010201     C     INZVAR        BEGSR
019400000801     C*
019500040802     C                   Z-ADD     *zeros        Num5_0            5 0
019600040802     C                   MOVEL     '0'           FlgCAS            1
019700000801     C*
019800000801     C                   ENDSR
019900000801     C*----------------------------------------------------*
020000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020100000801     C*----------------------------------------------------*
020200000801     C     DEFCAM        BEGSR
020300000801     C*
020400020619     C* Imposto i valori di default...
020500060529     C                   Z-ADD     *zeros        VABCCM
020600060529     C                   Z-ADD     *zeros        VATCCM
020700060529     C                   Z-ADD     097           VABLNP
020800060529     C                   Z-ADD     097           VATLNP
020900040714     C                   Z-ADD     000           VABCTR
021000060529     C                   MOVEL     '7Q'          VABCTM
021100060529     C                   MOVEL     'C'           VABTSP
021200040823     C                   MOVEL     '1'           VABCBO
021300020619     C* ... e poi verifico se sono stati passati come parametri
021400020619     C                   IF        vlrppt > *blanks
021500040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021700020619     C                   EXSR      CHKNUM
021800020619     C                   IF        PiInt=*on
021900020619     C                   Z-ADD     PiVal         VABCCM
022000020619     C                   Z-ADD     PiVal         VATCCM
022100020619     C                   ENDIF
022200040506     C                   ENDIF
022300040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022500020619     C                   EXSR      CHKNUM
022600020619     C                   IF        PiInt=*on
022700020619     C                   Z-ADD     PiVal         VABLNP
022800020619     C                   Z-ADD     PiVal         VATLNP
022900040506     C                   ENDIF
023000020619     C                   ENDIF
023100040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023300020619     C                   EXSR      CHKNUM
023400020619     C                   IF        PiInt=*on
023500020619     C                   Z-ADD     PiVal         VABCTR
023600040506     C                   ENDIF
023700020619     C                   ENDIF
023800020619     C                   ENDIF
023900000801     C*
024000000801     C                   ENDSR
024100000801     C*----------------------------------------------------*
024200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024300000801     C*----------------------------------------------------*
024400040823     C     IMPVAB        BEGSR
024500040823     C*
024600020305     C                   EXSR      INZVAR
024700020305     C                   EXSR      DEFCAM
024800010305     C*
024900000801     C                   Z-ADD     *zeros        errore            1 0
025000000830     C                   MOVEL     datcor        VABAAS
025100020305     C                   MOVEL     datcor        VATAAS
025200040526     C                   MOVE      datcor        VABMGS
025300060529     C                   MOVE(P)   vlrpoi        VABFGS
025400060529     C                   MOVE(P)   vlrpoi        VATFGS
025500050628     C*
025600060529     C                   EVAL      VABRSD=%trim(%subst(vindta:23:26))
025700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
025800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
025900020117     C* ==
026000060529     C                   EVAL      VABIND=%trim(%subst(vindta:49:26))
026100060529     C                   EVAL      VABLOD=%trim(%subst(vindta:86:24))
026200060529     C                   EVAL      VABPRD=%trim(%subst(vindta:110:2))
026300060529     C                   EVAL      VABRMA=%trim(%subst(vindta:8:9))
026400061026     C                   EVAL      VABTSP=%trim(%subst(vindta:151:1))
026500060307     C*
026600060307     C* Determino il tipo porto della spedizione corrente
026700060529     C                   IF        %subst(vindta:116:1) = 'F'
026800060307     C                   EVAL      VABCBO = '1'
026900060307     C                   ENDIF
027000060529     C                   IF        %subst(vindta:116:1) = 'A'
027100060307     C                   EVAL      VABCBO = '2'
027200060307     C                   ENDIF
027300060529     C* CCM
027400060529     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
027500060529     C                   EXSR      CHKNUM
027600060529     C                   IF        PiInt=*on
027700060529     C                   Z-ADD     PiVal         VABCCM
027800060608     C                   Z-ADD     PiVal         VATCCM
027900060529     C                   ELSE
028000060529     C                   ADD       1             errore
028100060529     C                   EVAL      vinmsg = %trimr(vinmsg)
028200060608     C                             + ' ' + 'VABCCM VATCCM'
028300060529     C                   ENDIF
028400060529     C* RMN
028500060529     C                   EVAL      PiStr=%trim(%subst(vindta:8:9))
028600060529     C                   EXSR      CHKNUM
028700060529     C                   IF        PiInt=*on
028800060529     C                   Z-ADD     PiVal         VABRMN
028900060529     C                   ELSE
029000060529     C                   ADD       1             errore
029100060529     C                   EVAL      vinmsg = %trimr(vinmsg)
029200060529     C                             + ' ' + 'VABRMN'
029300060529     C                   ENDIF
029400060529     C* NSP => Stacco un numeratore da AZNUM
029500060714     C                   if        curspe <> depspe
029600060529     C                   clear                   TRUL33DS
029700060529     C                   eval      I33OPE = *zeros
029800060529     C                   eval      I33CNU = 302
029900060529     C                   eval      I33NUM = 1
030000060529     C                   movel     TRUL33DS      KPJBU
030100060529     C                   call      'TRUL33R'
030200060529     C                   parm                    KPJBA
030300060529     C                   movel     KPJBU         TRUL33DS
030400060529     C                   if        O33ERR = *zeros
030500060529     C                   z-add     O33NRF        VABNSP
030600060529     C                   z-add     O33NRF        VATNSP
030700060529     C                   else
030800060529     C                   Z-ADD     1             errore
030900060529     C                   EVAL      vinmsg = %trimr(vinmsg)
031000060529     C                             + ' ' + 'VABNSP VATNSP'
031100060529     C                   endif
031200060714     C                   endif
031300060529     C* CAD
031400060529     C                   EVAL      PiStr=%trim(%subst(vindta:81:5))
031500010201     C                   EXSR      CHKNUM
031600010201     C                   IF        PiInt=*on
031700010201     C                   Z-ADD     PiVal         Num5_0
031800040506     C                   MOVEL(P)  Num5_0        VABCAD
031900010201     C                   ELSE
032000040506     C                   ADD       1             errore
032100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
032200040506     C                             + ' ' + 'VABCAD'
032300010201     C                   ENDIF
032400040506     C* Reperisco la provincia dal CAP e dalla località
032500040526     C                   IF        VABCAD <> *blanks AND
032600040526     C                             VABPRD  = *blanks
032700040506     C                   CLEAR                   TISI95DS
032800040506     C                   EVAL      I95TCN = '3'
032900040506     C                   Z-ADD     datcor        I95DAT
033000040506     C                   EVAL      I95CAP = VABCAD
033100040506     C                   EVAL      I95LOC = VABLOD
033200050627     C                   EVAL      I95NAR = VABNZD
033300040506     C                   CALL      'TISI95R'
033400040506     C                   PARM                    TISI95DS
033500040506     C                   EVAL      VABPRD = O95PRV
033600040506     C                   ENDIF
033700060307     C* NCL
033800060529     C                   ADD       1             VABNCL
033900040506     C* PKB
034000060529     C                   EVAL      PiStr=%trim(%subst(vindta:142:9))
034100010201     C                   EXSR      CHKNUM
034200010201     C                   IF        PiNum=*on
034300060529     C                   EVAL      Pival = Pival/1000                           * gestisco 3 decim.
034400060529     C                   ADD(H)    PiVal         VABPKB
034500010201     C                   ELSE
034600010201     C                   ADD       1             errore
034700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034800010201     C                             + ' ' + 'VABPKB'
034900010201     C                   ENDIF
035000060307     C* CAS
035100060529     C                   IF        %subst(vindta:117:12) <> *zeros
035200060529     C                   EVAL      FlgCAS='1'
035300060529     C                   EVAL      VABVCA='EUR'
035400060307     C                   ENDIF
035500060529     C                   EVAL      PiStr=%trim(%subst(vindta:117:12))
035600060307     C                   EXSR      CHKNUM
035700060307     C                   IF        PiNum=*on
035800060307     C                   EVAL(H)   VABCAS=PiVal/100                             * gestisco 2 dec.
035900060307     C                   ELSE
036000060307     C                   ADD       1             errore
036100060307     C                   EVAL      vinmsg = %trimr(vinmsg)
036200060307     C                             + ' ' + 'VABCAS'
036300060307     C                   ENDIF
036400060529     C*
036500060529     C* Gestione Disk C
036600060529     C                   exsr      exeVATE
036700010205     C*
036800010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
036900040802     C                   IF        FlgCAS <> '0'
037000010205     C                   EVAL      VABCBO = '4'
037100010205     C                   ELSE
037200010205     C                   EVAL      VABCBO = '1'
037300010205     C                   ENDIF
037400020305     C*
037500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
037600011113     C                   EXSR      CHKIMPDIV
037700010202     C*
037800000801     C* Ebbene...
037900000801     C                   ADD       1             §CTRMO
038000010201     C                   IF        errore <> *zeros
038100000801     C                   ADD       1             §CTRNO
038200000801     C                   EVAL      vinflg = '2'
038300000801     C                   ELSE
038400010201     C                   ADD       1             §CTROKVB
038500000801     C                   ENDIF
038600000801     C*
038700000801     C                   ENDSR
038800050628     C*----------------------------------------------------*
038900060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
039000050628     C*----------------------------------------------------*
039100060307     C     EXEVATE       BEGSR
039200050628     C*
039300050628     C* Valorizzo il barcode
039400050628     C                   EVAL      VATTRC='E'
039500060529     C                   EVAL      VATNOT=%trim(%subst(vindta:129:13))
039600050628     C                   exsr      wrivat                                       => scarico VAT
039700050628     C*
039800050628     C                   ENDSR
039900010201     C*----------------------------------------------------*
040000040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
040100010201     C*----------------------------------------------------*
040200020305     C     WRIVAT        BEGSR
040300050628     C*
040400060223     C* Scrivo solo se valorizzato qualcosa
040500060223     C                   IF        VATNOT <> *blanks
040600040802     C                   WRITE     FIVAT000
040700060223     C                   ENDIF
040800010201     C*
040900010201     C                   ENDSR
041000010202     C*----------------------------------------------------*
041100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
041200010202     C*----------------------------------------------------*
041300020305     C     PREVAT        BEGSR
041400010202     C*
041500021113     C* Compongo il nome del membro da dare al FIVATWWR
041600010202     C                   eval      parmbr = vlrhdl
041700010202     C                   movel     'M'           parmbr
041800050627     C                   eval      parccm = %subst(vlrKSC:2:7)
041900010202     C                   eval      paropz = '1'
042000010202     C* Effettuo la chiamata al CLLE preposto
042100040506     C                   call(e)   'TITVVTC'
042200010202     C                   parm                    parccm
042300010202     C                   parm                    parmbr
042400010202     C                   parm                    paropz
042500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
042600010202     C                   if        %error
042700010202     C                   movel     '1'           chkcall
042800010202     C                   else
042900010202     C                   movel     '0'           chkcall
043000010202     C                   endif
043100010202     C*
043200010202     C                   ENDSR
043300000801     C*----------------------------------------------------*
043400000801     C*  CONTROLLO NUMERICITA' CAMPI
043500000801     C*----------------------------------------------------*
043600000801     C     CHKNUM        BEGSR
043700000801     C*
043800000801     C                   call(e)   'ISNUMERIC'
043900000801     C                   PARM                    PiStr            30
044000050627     C                   PARM      ','           PiDecChr          1
044100000801     C                   PARM      *ZEROS        PiVal            30 9
044200000801     C                   PARM      '0'           PiInt             1
044300000801     C                   PARM      '0'           PiNum             1
044400000801     C                   IF        %error
044500000801     C                   EVAL      PiInt=*off
044600000801     C                   ENDIF
044700000801     C*
044800000801     C                   ENDSR
044900000801     C***
045000000801
045100011113
045200011113     C*----------------------------------------------------*
045300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
045400011113     C*----------------------------------------------------*
045500011113     C     CHKIMPDIV     BEGSR
045600011113     C*
045700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
045800011113     C                   Z-ADD     *zeros        wrkDec            9 9
045900011113     C*
046000011113     C* Come prima cosa effettuo considerazioni sulla divisa
046100011113     C                   IF        vabIAS > *zeros
046200011113     C                   IF        vabVAS <> 'EUR'
046300011113     C                   EVAL      vabVAS =  'ITL'
046400011113     C                   ENDIF
046500011113     C                   ENDIF
046600011113     C*
046700011113     C                   IF        vabCAS > *zeros
046800011113     C                   IF        vabVCA <> 'EUR'
046900011113     C                   EVAL      vabVCA =  'ITL'
047000011113     C                   ENDIF
047100011113     C                   ENDIF
047200011113     C*
047300011113     C                   IF        vabVMD > *zeros
047400020305     C                   IF        vabVAD <> 'EUR'
047500011113     C                   EVAL      vabVAD =  'ITL'
047600011113     C                   ENDIF
047700011113     C                   ENDIF
047800011113     C*
047900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
048000011113     C                   Z-ADD     vabIAS        wrkDec
048100011113     C                   IF        wrkDec > *zeros
048200011113     C                   IF        vabVAS = 'ITL'
048300011113     C                   EVAL      vabIAS = *zeros
048400011113     C                   ENDIF
048500011113     C                   ENDIF
048600011113     C*
048700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
048800011113     C                   Z-ADD     vabCAS        wrkDec
048900011113     C                   IF        wrkDec > *zeros
049000011113     C                   IF        vabVCA = 'ITL'
049100011113     C                   EVAL      vabCAS = *zeros
049200011113     C                   ENDIF
049300011113     C                   ENDIF
049400011113     C*
049500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
049600011113     C                   Z-ADD     vabVMD        wrkDec
049700011113     C                   IF        wrkDec > *zeros
049800011113     C                   IF        vabVAD = 'ITL'
049900011113     C                   EVAL      vabVMD = *zeros
050000011113     C                   ENDIF
050100011113     C                   ENDIF
050200011113     C*
050300011113     C                   ENDSR
050400011113     C***
050500011113
050600011113
050700000801
050800000801
050900990920      /TITLE Invio dei dati al punto operativo.
051000010202     C     invio         BEGSR
051100990920     C*
051200021113     C* 1° invio FIVAT
051300010201     C                   reset                   dscmz
051400010201     C                   move      vlrpoi        cmzdst
051500021113     C                   eval      cmzfld = 'FIVATWWR'
051600010201     C                   eval      cmzmbd = vlrhdl
051700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
051800021009     C***                if        prmfir = *blanks
051900021113     C                   eval      cmzfla = 'FIVAT00F'
052000021113     C                   eval      cmzmba = 'FIVAT00F'
052100021009     C***                else
052200021009     C***                eval      cmzfla = prmfir
052300021009     C***                eval      cmzmba = prmfir
052400021009     C***                endif
052500010201     C                   eval      cmznrr = *zeros
052600020305     C                   move      §ctrokvt      cmznrr
052700021018     C                   eval      cmzlba = vlrfl1
052800010201     C                   call(e)   'TIS711C'
052900010201     C                   parm                    dscmz
053000010201     C                   parm      *blanks       esito
053100010205     C                   if        %error
053200010205     C                             or cmzerr = '1'
053300010205     C                             or esito  = '1'
053400010205     C                   eval      wrkesito = '3'
053500010205     C                   else
053600010201     C*
053700021113     C* 2° invio FIVAB
053800010201     C                   reset                   dscmz
053900010201     C                   move      vlrpoi        cmzdst
054000010201     C                   eval      cmzfld = vlrfou
054100010201     C                   eval      cmzmbd = vlrhdl
054200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
054300021009     C***                if        prmfir = *blanks
054400021113     C                   eval      cmzfla = 'FIVAB00F'
054500021113     C                   eval      cmzmba = 'FIVAB00F'
054600021009     C***                else
054700021009     C***                eval      cmzfla = prmfir
054800021009     C***                eval      cmzmba = prmfir
054900021009     C***                endif
055000010201     C                   eval      cmznrr = *zeros
055100010201     C                   move      §ctrokvb      cmznrr
055200021018     C                   eval      cmzlba = vlrfl1
055300010201     C                   call(e)   'TIS711C'
055400010201     C                   parm                    dscmz
055500010201     C                   parm      *blanks       esito
055600010201     C                   if        %error
055700010201     C                             or cmzerr = '1'
055800010201     C                             or esito  = '1'
055900010201     C                   eval      wrkesito = '3'
056000010201     C                   endif
056100010205     C                   endif
056200990920     C*
056300000613     C                   ENDSR
056400000613     C***
056500070411
056600070411     C     *pssr         BEGSR
056700070411     C*
056800070411     C                   if        %open(tivin00r)
056900070411     C                   close     tivin00r
057000070411     C                   endif
057100070411     C                   if        %open(fivabwwr)
057200070411     C                   close     fivabwwr
057300070411     C                   endif
057400070411     C                   if        %open(fivatwwr)
057500070411     C                   close     fivatwwr
057600070411     C                   endif
057700070411     C*
057800070411     C* Effettuo la chiamata al CLLE preposto
057900070411     C                   call(e)   'TITVVTC'
058000070411     C                   parm                    parccm
058100070411     C                   parm                    parmbr
058200070411     C                   parm      '2'           paropz
058300070411     C*
058400070411     C                   eval      wrkesito = '2'
058500070411     C*
058600070411     C                   seton                                        LR
058700070411     C*
058800070411     C                   ENDSR     '*CANCL'
058900070411     C***
059000070411
059100990910
059200000613     C     *inzsr        BEGSR
059300990910     C*
059400990910     C     *entry        plist
059500990920     C                   parm                    tivlrds
059600990921     C                   parm      wrkesito      esito
059700000724     C                   parm                    prmlit
059800000710     C                   parm                    prmfir
059900000613     C*
060000000830     C* CALCOLA LA DATA CORRENTE
060100000830     C                   time                    wn14             14 0
060200000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
060300000830     C                   z-add     wn8           g08dat
060400000830     C                   z-add     *zeros        g08inv
060500000830     C                   movel     '0'           g08err
060600000830     C                   call      'XSRDA8'
060700000830     C                   parm                    wlbda8
060800000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
060900000830     C*
061000000613     C                   ENDSR
061100000613     C***
