000100090729      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500090729     FFIVABwwr  O    E             DISK    usropn
000600090729     FFIVATwwr  O    E             DISK    usropn
000700000801     D*----------------------------------------------------
000800000801     D* DICHIARAZIOINE VARIABILI DI WRK
000900000801     D*----------------------------------------------------
001000990920     D dscmz         e ds                  inz
001100990910     D psds           sds
001200990910     D  procname         *PROC
001300990920     D tivlrds       e ds                  extname(tivlr00f)
001400090730     D tisi95ds      e ds
001500990910     D esito           s              1
001600000724     D prmlit          s             10
001700000710     D prmfir          s             10
001800990921     D wrkesito        s                   like(esito)
001900000613     D rrnum           s              6  0 INZ(*zeros)
002000090729     D depspe          s             16    INZ(*blanks)
002100090729     D curspe          s             16    INZ(*blanks)
002200090804     D contaRec        s              6  0 INZ(*zeros)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700010213
002800000801     D  Num5_0         s              5  0
002900030107     D  Num7_0         s              7  0
003000000830
003100000830     D*------------------
003200000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003300000830     D*------------------
003400000830     D WLBDA8          DS                  INZ
003500000830     D  G08DAT                 1      8  0
003600000830     D  G08INV                 9     16  0
003700000830     D  G08ERR                17     17
003800000830     D  G08TGI                18     22  0
003900000830     D*
004000990908
004100010201
004200010201
004300000913     C                   reset                   rrnum
004400990921     C                   reset                   esito
004500990921     C                   reset                   wrkesito
004600000613     C*
004700050802     C                   EXSR      RWVAB                                        LETT/SCR. VAB/VAT
004800000613     C*
004900010202     C* Effettuo la chiamata al CLLE preposto
005000090729     C                   call(e)   'TITVVTC'
005100010202     C                   parm                    parccm
005200010202     C                   parm                    parmbr
005300010202     C                   parm      '2'           paropz
005400090730     C*
005500090730     C* Effettuo lancio TISI95 solo x chiusura
005600090730     C                   CLEAR                   TISI95DS
005700090730     C                   EVAL      I95TLA = 'C'
005800090730     C                   CALL      'TISI95R'
005900090730     C                   PARM                    TISI95DS
006000000616     C*
006100000801     C
006200010201     C                   seton                                        LR
006300990908
006400000801
006500910830     C*--------------------------------------------------------
006600090729     C* RWVAB     LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR *
006700910830     C*--------------------------------------------------------
006800050802     C     RWVAB         BEGSR
006900990910     C*
007000990914     C                   if        not %open(tivin00r)
007100990908     C                   open      tivin00r
007200990914     C                   endif
007300090729     C                   if        not %open(fivabwwr)
007400090729     C                   open      fivabwwr
007500990914     C                   endif
007600090729     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007700020305     C                   exsr      prevat
007800010201     C*
007900040428     C                   if        chkcall = '0'
008000010202     C*
008100090729     C                   if        not %open(fivatwwr)
008200090729     C                   open      fivatwwr
008300010201     C                   endif
008400990910     C*
008500010201     C                   clear                   §CTROKVB          5 0
008600020305     C                   clear                   §CTROKVT          5 0
008700000801     C                   clear                   §CTRMO            5 0
008800000801     C                   clear                   §CTRNO            5 0
008900090729     C*
009000090729     C                   exsr      INZVAR
009100090729     C                   exsr      DEFCAM
009200990910     C*
009300921023     C                   DO        *HIVAL
009400990913     C*
009500990915     C                   READ      tivin00r                               70
009600000905     C                   if        vindta > *blanks
009700000613     C                   add       1             rrnum
009800000801     C*
009900000801     C                   if        *in70 = *off
010000000801     C                             and
010100000801     C                             (vinflg = *blanks
010200000801     C                              or vinflg = '0'
010300000801     C                              or vinflg = '2')
010400000801     C*
010500000801     C                   clear                   vinmsg
010600090729     C*
010700090729     C                   if        contaRec = *zeros
010800090729     C                   exsr      impvab
010900090729     C                   endif
011000090729     C                   eval      contaRec = contaRec + 1
011100010305     C*
011200090729     C* Salvo il numero speidzione corrente
011300090729     C                   eval      curspe = %subst(vindta:308:16)
011400121001     C                   if        depspe = *blanks                             => 1° giro
011500121001     C                   eval      depspe = curspe                              => memorizz. spediz
011600121001     C                   endif
011700090729     C*
011800090729     C* Gestisco la rottura di codice x numero speidzione
011900121001     C                   if        curspe <> depspe
012000090729     C                   exsr      wrivab                                       => scrivo VAB
012100090729     C                   exsr      INZVAR
012200090729     C                   exsr      DEFCAM
012300090729     C                   exsr      impvab                                       => carico VAB
012400090729     C                   eval      depspe = curspe
012500090729     C                   endif
012600090729     C*
012700090729     C* Scrivo sempre il "chi sono" corrente
012800090729     C                   exsr      wrivat
012900090729     C*
013000010305     C                   endif
013100000905     C*
013200000905     C                   else
013300000905     C                   eval      vinflg = '1'
013400000905     C                   endif
013500000905     C*
013600000905     C  N70              update    tivin000
013700000905     C*
013800991022     C  N70              ENDdo
013900010202     C*
014000010202     C                   endif
014100090729     C*
014200090729     C* Scarico la testata ancora in canna
014300090729     C                   exsr      wrivab                                       => scrivo VAB
014400050802     C*
014500990910     C* Se non ci sono record con errori ...
014600000710     C                   if        §ctrno = 0
014700990910     C* ... restituisco esito OK.
014800990921     C                   eval      wrkesito = '0'
014900990910     C                   else
015000010201     C                   if        §ctrokvb > 0
015100990921     C                   eval      wrkesito = '1'
015200000710     C                   else
015300000710     C                   eval      wrkesito = '2'
015400990910     C                   endif
015500000710     C                   endif
015600990910     C*
015700990914     C                   if        %open(tivin00r)
015800990908     C                   close     tivin00r
015900990914     C                   endif
016000090729     C                   if        %open(fivabwwr)
016100090729     C                   close     fivabwwr
016200990914     C                   endif
016300090729     C                   if        %open(fivatwwr)
016400090729     C                   close     fivatwwr
016500010201     C                   endif
016600021017     C*
016700021017     C                   if        vlrpoi <> 999
016800021017     C                   eval      vlrpoi = vlrpoi
016900021017     C                   endif
017000990910     C*
017100010201     C                   if        §ctrokvb > 0
017200021017     C                             and vlrpoi > *zeros
017300010202     C                   exsr      invio
017400990920     C                   endif
017500990920     C*
017600910830     C                   ENDSR
017700000613     C***
017800010305
017900010305     C*----------------------------------------------------*
018000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
018100010305     C*----------------------------------------------------*
018200020305     C     WRIVAB        BEGSR
018300010305     C*
018400090729     C                   write     fivab000                                     => scarico il VAB
018500010305     C*
018600010305     C                   ENDSR
018700990920
018800000801     C*----------------------------------------------------*
018900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019000000801     C*----------------------------------------------------*
019100010201     C     INZVAR        BEGSR
019200000801     C*
019300010201     C                   Z-ADD     *zeros        Num5_0
019400030107     C                   Z-ADD     *zeros        Num7_0
019500030711     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
019600030711     C                   MOVEL     '0'           FlgCAS            1
019700090729     C*
019800000801     C                   ENDSR
019900000801     C*----------------------------------------------------*
020000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020100000801     C*----------------------------------------------------*
020200000801     C     DEFCAM        BEGSR
020300040428     C*
020400090729     C                   CLEAR                   FIVAB000
020500090729     C                   CLEAR                   FIVAB000
020600000801     C*
020700020619     C* Imposto i valori di default...
020800090729     C                   Z-ADD     1201624       VABCCM
020900090729     C                   Z-ADD     1201624       VATCCM
021000021017     C                   Z-ADD     vlrpoi        VABFGS
021100021017     C                   Z-ADD     vlrpoi        VATFGS
021200090729     C                   Z-ADD     120           VABLNP
021300090729     C                   Z-ADD     120           VATLNP
021400050802     C                   Z-ADD     000           VABCTR
021500021017     C                   MOVEL     '7Q'          VABCTM
021600050802     C                   MOVEL     '1'           VABCBO
021700020619     C* ... e poi verifico se sono stati passati come parametri
021800020619     C                   IF        vlrppt > *blanks
021900040428     C                   IF        %subst(vlrppt:1:7) <> *blanks
022000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022100020619     C                   EXSR      CHKNUM
022200020619     C                   IF        PiInt=*on
022300020619     C                   Z-ADD     PiVal         VABCCM
022400020619     C                   Z-ADD     PiVal         VATCCM
022500020619     C                   ENDIF
022600040428     C                   ENDIF
022700040428     C                   IF        %subst(vlrppt:8:3) <> *blanks
022800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022900020619     C                   EXSR      CHKNUM
023000020619     C                   IF        PiInt=*on
023100020619     C                   Z-ADD     PiVal         VABLNP
023200020619     C                   Z-ADD     PiVal         VATLNP
023300020619     C                   ENDIF
023400040428     C                   ENDIF
023500040428     C                   IF        %subst(vlrppt:11:3) <> *blanks
023600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023700020619     C                   EXSR      CHKNUM
023800020619     C                   IF        PiInt=*on
023900020619     C                   Z-ADD     PiVal         VABCTR
024000020619     C                   ENDIF
024100040428     C                   ENDIF
024200020619     C                   ENDIF
024300000801     C*
024400000801     C                   ENDSR
024500000801     C*----------------------------------------------------*
024600090729     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024700000801     C*----------------------------------------------------*
024800010201     C     IMPVAB        BEGSR
024900010305     C*
025000000801     C                   Z-ADD     *zeros        errore            1 0
025100000830     C                   MOVEL     datcor        VABAAS
025200020305     C                   MOVEL     datcor        VATAAS
025300000830     C                   MOVE      datcor        VABMGS
025400031117     C*
025500050802     C                   EVAL      VABRSD=%trim(%subst(vindta:53:35))
025600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
025700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
025800020117     C* ==
025900050802     C                   EVAL      VABRD2=%trim(%subst(vindta:88:35))
026000050802     C                   EVAL      VABIND=%trim(%subst(vindta:123:35))
026100050802     C                   EVAL      VABLOD=%trim(%subst(vindta:167:25))
026200050802     C                   EVAL      VABPRD=%trim(%subst(vindta:202:2))
026300050802     C                   EVAL      VABRMA=%trim(%subst(vindta:324:15))
026400050802     C                   EVAL      VABNOT=%trim(%subst(vindta:380:35))
026500050802     C                   EVAL      VABNT2=%trim(%subst(vindta:415:35))
026600090729     C                   EVAL      VABNAS=%trim(%subst(vindta:234:15))
026700090730     C                   EVAL      VABTIC=%trim(%subst(vindta:301:2))
026800051117     C                   IF        %trim(%subst(vindta:47:2)) = '2'
026900051117     C                   EVAL      VABCBO='2'
027000051117     C                   ENDIF
027100030711     C* CAD
027200050802     C                   EVAL      PiStr=%trim(%subst(vindta:158:9))
027300010201     C                   EXSR      CHKNUM
027400010201     C                   IF        PiInt=*on
027500010201     C                   Z-ADD     PiVal         Num5_0
027600021017     C                   MOVEL     Num5_0        VABCAD
027700010201     C                   ELSE
027800010201     C                   ADD       1             errore
027900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
028000021017     C                             + ' ' + 'VABCAD'
028100010201     C                   ENDIF
028200090730     C*
028300090730     C* Se provincia nn valorizzata la reperisco
028400090730     C* tramite TISI95R a seconda dei dati d instradamento presenti
028500090730     C                   IF        VABPRD = *blanks
028600090730     C                   CLEAR                   TISI95DS
028700090730     C                   EVAL      I95TCN = '3'
028800090730     C                   Z-ADD     datcor        I95DAT
028900090730     C                   EVAL      I95NAR = VABNZD
029000090730     C                   EVAL      I95CAP = VABCAD
029100090730     C                   EVAL      I95LOC = VABLOD
029200090730     C                   CALL      'TISI95R'
029300090730     C                   PARM                    TISI95DS
029400090730     C                   EVAL      VABPRD = O95PRV
029500090730     C                   ENDIF
029600120730     C* CCM
029700120730     C                   IF        %subst(vindta:3:7) <> *all'0' AND
029800120730     C                             %subst(vindta:3:7) <> *blanks
029900120730     C                   EVAL      PiStr=%trim(%subst(vindta:3:7))
030000120730     C                   EXSR      CHKNUM
030100120730     C                   IF        PiInt=*on
030200120730     C                   Z-ADD     PiVal         VABCCM
030300120906     C                   Z-ADD     PiVal         VATCCM
030400120730     C                   ELSE
030500120730     C                   ADD       1             errore
030600120730     C                   EVAL      vinmsg = %trimr(vinmsg)
030700120906     C                             + ' ' + 'VABCCM VATCCM'
030800120730     C                   ENDIF
030900120730     C                   ENDIF
031000120730     C* LNP
031100120730     C                   IF        %subst(vindta:11:3) <> *all'0' AND
031200120730     C                             %subst(vindta:11:3) <> *blanks
031300120730     C                   EVAL      PiStr=%trim(%subst(vindta:11:3))
031400120730     C                   EXSR      CHKNUM
031500120730     C                   IF        PiInt=*on
031600120730     C                   Z-ADD     PiVal         VABLNP
031700120906     C                   Z-ADD     PiVal         VATLNP
031800120730     C                   ELSE
031900120730     C                   ADD       1             errore
032000120730     C                   EVAL      vinmsg = %trimr(vinmsg)
032100120906     C                             + ' ' + 'VABLNP VATLNP'
032200120730     C                   ENDIF
032300120730     C                   ENDIF
032400030711     C* RMN
032500090729     C                   EVAL      PiStr=%trim(%subst(vindta:308:16))
032600010201     C                   EXSR      CHKNUM
032700010201     C                   IF        PiInt=*on
032800021017     C                   Z-ADD     PiVal         VABRMN
032900010201     C                   ELSE
033000010201     C                   ADD       1             errore
033100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
033200050802     C                             + ' ' + 'VABRMN'
033300010201     C                   ENDIF
033400050802     C* NSP
033500090729     C                   EVAL      PiStr=%trim(%subst(vindta:311:7))
033600050802     C                   EXSR      CHKNUM
033700050802     C                   IF        PiInt=*on
033800050802     C                   Z-ADD     PiVal         Num7_0
033900050802     C                   MOVE(P)   Num7_0        VABNSP
034000050802     C                   MOVE(P)   Num7_0        VATNSP
034100050802     C                   ELSE
034200050802     C                   ADD       1             errore
034300050802     C                   EVAL      vinmsg = %trimr(vinmsg)
034400050802     C                             + ' ' + 'VABNSP VATNSP'
034500050802     C                   ENDIF
034600050802     C* NCL
034700050802     C                   EVAL      PiStr=%trim(%subst(vindta:249:6))
034800030107     C                   EXSR      CHKNUM
034900030107     C                   IF        PiInt=*on
035000040428     C                   ADD       PiVal         VABNCL
035100030107     C                   ELSE
035200030107     C                   ADD       1             errore
035300030107     C                   EVAL      vinmsg = %trimr(vinmsg)
035400030107     C                             + ' ' + 'VABNCL'
035500030107     C                   ENDIF
035600050802     C* PKB
035700050802     C                   EVAL      PiStr=%trim(%subst(vindta:255:9))
035800030107     C                   EXSR      CHKNUM
035900030107     C                   IF        PiNum=*on
036000090928     C***                EVAL      PiVal = PiVal / 1000                         * da grammi a Kg.
036100050802     C                   Z-ADD     PiVal         VABPKB
036200030107     C                   ELSE
036300030107     C                   ADD       1             errore
036400030107     C                   EVAL      vinmsg = %trimr(vinmsg)
036500030107     C                             + ' ' + 'VABPKB'
036600030107     C                   ENDIF
036700090730     C* IAS
036800090730     C                   IF        %subst(vindta:216:15) <> *blanks
036900090730     C                   EVAL      PiStr=%trim(%subst(vindta:216:15))
037000090730     C                   EXSR      CHKNUM
037100090730     C                   IF        PiNum=*on
037200090730     C                   Z-ADD     PiVal         VABIAS
037300090730     C                   IF        VABIAS > *zeros
037400090730     C                   EVAL      VABVAS = 'EUR'
037500090730     C                   ENDIF
037600090730     C                   ELSE
037700090730     C                   ADD       1             errore
037800090730     C                   EVAL      vinmsg = %trimr(vinmsg)
037900090730     C                             + ' ' + 'VABIAS'
038000090730     C                   ENDIF
038100090730     C                   ENDIF
038200090730     C* CAS
038300090730     C                   IF        %subst(vindta:286:15) <> *blanks
038400090730     C                   MOVEL     '1'           FlgCAS
038500090730     C                   EVAL      PiStr=%trim(%subst(vindta:286:15))
038600090730     C                   EVAL      VABVCA=%trim(%subst(vindta:303:3))
038700050802     C                   EXSR      CHKNUM
038800050802     C                   IF        PiNum=*on
038900090730     C                   Z-ADD     PiVal         VABCAS
039000050802     C                   ELSE
039100050802     C                   ADD       1             errore
039200050802     C                   EVAL      vinmsg = %trimr(vinmsg)
039300090730     C                             + ' ' + 'VABCAS'
039400050802     C                   ENDIF
039500050802     C                   ENDIF
039600010205     C*
039700010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
039800051117     C                   IF        FlgCAS <> '0'
039900051117     C                   IF        VABCBO = '1'
040000040428     C                   EVAL      VABCBO = '4'                                 * franco  SI contr.
040100051117     C                   ENDIF
040200051117     C                   IF        VABCBO = '2'
040300051117     C                   EVAL      VABCBO = '6'                                 * franco  SI contr.
040400051117     C                   ENDIF
040500031117     C                   ENDIF
040600020305     C*
040700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
040800011113     C                   EXSR      CHKIMPDIV
040900010202     C*
041000000801     C* Ebbene...
041100000801     C                   ADD       1             §CTRMO
041200010201     C                   IF        errore <> *zeros
041300000801     C                   ADD       1             §CTRNO
041400000801     C                   EVAL      vinflg = '2'
041500000801     C                   ELSE
041600010201     C                   ADD       1             §CTROKVB
041700000801     C                   ENDIF
041800000801     C*
041900000801     C                   ENDSR
042000010201     C*----------------------------------------------------*
042100090729     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
042200010201     C*----------------------------------------------------*
042300020305     C     WRIVAT        BEGSR
042400090729     C*
042500090729     C                   eval      vatAAS = vabAAS
042600090729     C                   eval      vatLNP = vabLNP
042700090729     C                   eval      vatNRS = vabNRS
042800090729     C                   eval      vatNSP = vabNSP
042900090729     C                   eval      vatTRC = 'E'
043000010201     C*
043100090729     C* Valorizzo il buffer di scrittura del FIVAT
043200090804     C                   eval      vatNOT = %trim(%subst(vindta:28:19))
043300090729     C                   write     FIVAT000
043400040428     C*
043500050802     C                   add       1             §ctrokvt
043600010201     C*
043700010201     C                   ENDSR
043800010202     C*----------------------------------------------------*
043900090729     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044000010202     C*----------------------------------------------------*
044100020305     C     PREVAT        BEGSR
044200010202     C*
044300090729     C* Compongo il nome del membro da dare al FIVATWWR
044400010202     C                   eval      parmbr = vlrhdl
044500010202     C                   movel     'M'           parmbr
044600050802     C                   eval      parccm = '00051381'
044700010202     C                   eval      paropz = '1'
044800010202     C* Effettuo la chiamata al CLLE preposto
044900090729     C                   call(e)   'TITVVTC'
045000010202     C                   parm                    parccm
045100010202     C                   parm                    parmbr
045200010202     C                   parm                    paropz
045300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045400010202     C                   if        %error
045500010202     C                   movel     '1'           chkcall
045600010202     C                   else
045700010202     C                   movel     '0'           chkcall
045800010202     C                   endif
045900010202     C*
046000010202     C                   ENDSR
046100000801     C*----------------------------------------------------*
046200000801     C*  CONTROLLO NUMERICITA' CAMPI
046300000801     C*----------------------------------------------------*
046400000801     C     CHKNUM        BEGSR
046500000801     C*
046600000801     C                   call(e)   'ISNUMERIC'
046700000801     C                   PARM                    PiStr            30
046800090918     C                   PARM      '.'           PiDecChr          1
046900000801     C                   PARM      *ZEROS        PiVal            30 9
047000000801     C                   PARM      '0'           PiInt             1
047100000801     C                   PARM      '0'           PiNum             1
047200000801     C                   IF        %error
047300000801     C                   EVAL      PiInt=*off
047400030107     C                   EVAL      PiNum=*off
047500000801     C                   ENDIF
047600000801     C*
047700000801     C                   ENDSR
047800000801     C***
047900000801
048000011113
048100011113     C*----------------------------------------------------*
048200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
048300011113     C*----------------------------------------------------*
048400011113     C     CHKIMPDIV     BEGSR
048500011113     C*
048600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
048700011113     C                   Z-ADD     *zeros        wrkDec            9 9
048800011113     C*
048900011113     C* Come prima cosa effettuo considerazioni sulla divisa
049000011113     C                   IF        vabIAS > *zeros
049100011113     C                   IF        vabVAS <> 'EUR'
049200011113     C                   EVAL      vabVAS =  'ITL'
049300011113     C                   ENDIF
049400011113     C                   ENDIF
049500011113     C*
049600011113     C                   IF        vabCAS > *zeros
049700011113     C                   IF        vabVCA <> 'EUR'
049800011113     C                   EVAL      vabVCA =  'ITL'
049900011113     C                   ENDIF
050000011113     C                   ENDIF
050100011113     C*
050200011113     C                   IF        vabVMD > *zeros
050300020305     C                   IF        vabVAD <> 'EUR'
050400011113     C                   EVAL      vabVAD =  'ITL'
050500011113     C                   ENDIF
050600011113     C                   ENDIF
050700011113     C*
050800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
050900011113     C                   Z-ADD     vabIAS        wrkDec
051000011113     C                   IF        wrkDec > *zeros
051100011113     C                   IF        vabVAS = 'ITL'
051200011113     C                   EVAL      vabIAS = *zeros
051300011113     C                   ENDIF
051400011113     C                   ENDIF
051500011113     C*
051600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
051700011113     C                   Z-ADD     vabCAS        wrkDec
051800011113     C                   IF        wrkDec > *zeros
051900011113     C                   IF        vabVCA = 'ITL'
052000011113     C                   EVAL      vabCAS = *zeros
052100011113     C                   ENDIF
052200011113     C                   ENDIF
052300011113     C*
052400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
052500011113     C                   Z-ADD     vabVMD        wrkDec
052600011113     C                   IF        wrkDec > *zeros
052700011113     C                   IF        vabVAD = 'ITL'
052800011113     C                   EVAL      vabVMD = *zeros
052900011113     C                   ENDIF
053000011113     C                   ENDIF
053100011113     C*
053200011113     C                   ENDSR
053300011113     C***
053400021017
053500000801
053600000801
053700990920      /TITLE Invio dei dati al punto operativo.
053800010202     C     invio         BEGSR
053900990920     C*
054000090729     C* 1° invio FIVAT
054100010201     C                   reset                   dscmz
054200021017     C                   move      vlrpoi        cmzdst
054300090729     C                   eval      cmzfld = 'FIVATWWR'
054400010201     C                   eval      cmzmbd = vlrhdl
054500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
054600021009     C***                if        prmfir = *blanks
054700090729     C                   eval      cmzfla = 'FIVAT00F'
054800090729     C                   eval      cmzmba = 'FIVAT00F'
054900021009     C***                else
055000021009     C***                eval      cmzfla = prmfir
055100021009     C***                eval      cmzmba = prmfir
055200021009     C***                endif
055300010201     C                   eval      cmznrr = *zeros
055400020305     C                   move      §ctrokvt      cmznrr
055500021018     C                   eval      cmzlba = vlrfl1
055600010201     C                   call(e)   'TIS711C'
055700010201     C                   parm                    dscmz
055800010201     C                   parm      *blanks       esito
055900010205     C                   if        %error
056000010205     C                             or cmzerr = '1'
056100010205     C                             or esito  = '1'
056200010205     C                   eval      wrkesito = '3'
056300010205     C                   else
056400010201     C*
056500090729     C* 2° invio FIVAB
056600010201     C                   reset                   dscmz
056700021017     C                   move      vlrpoi        cmzdst
056800010201     C                   eval      cmzfld = vlrfou
056900010201     C                   eval      cmzmbd = vlrhdl
057000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057100021009     C***                if        prmfir = *blanks
057200090729     C                   eval      cmzfla = 'FIVAB00F'
057300090729     C                   eval      cmzmba = 'FIVAB00F'
057400021009     C***                else
057500021009     C***                eval      cmzfla = prmfir
057600021009     C***                eval      cmzmba = prmfir
057700021009     C***                endif
057800010201     C                   eval      cmznrr = *zeros
057900010201     C                   move      §ctrokvb      cmznrr
058000021018     C                   eval      cmzlba = vlrfl1
058100010201     C                   call(e)   'TIS711C'
058200010201     C                   parm                    dscmz
058300010201     C                   parm      *blanks       esito
058400010201     C                   if        %error
058500010201     C                             or cmzerr = '1'
058600010201     C                             or esito  = '1'
058700010201     C                   eval      wrkesito = '3'
058800010201     C                   endif
058900010205     C                   endif
059000990920     C*
059100000613     C                   ENDSR
059200000613     C***
059300070502
059400070502
059500070502
059600070502     C     *pssr         BEGSR
059700070502     C*
059800090729     C                   if        %open(fivabwwr)
059900090729     C                   close     fivabwwr
060000070502     C                   endif
060100090729     C                   if        %open(fivatwwr)
060200090729     C                   close     fivatwwr
060300070502     C                   endif
060400070502     C*
060500070502     C* Effettuo la chiamata al CLLE preposto
060600090729     C                   call(e)   'TITVVTC'
060700070502     C                   parm                    parccm
060800070502     C                   parm                    parmbr
060900070502     C                   parm      '2'           paropz
061000070502     C*
061100070502     C                   eval      wrkesito = '2'
061200070502     C*
061300070502     C                   seton                                        LR
061400070502     C*
061500070502     C                   ENDSR     '*CANCL'
061600070502     C***
061700070502
061800070502
061900990910
062000000613     C     *inzsr        BEGSR
062100990910     C*
062200990910     C     *entry        plist
062300990920     C                   parm                    tivlrds
062400990921     C                   parm      wrkesito      esito
062500000724     C                   parm                    prmlit
062600000710     C                   parm                    prmfir
062700000613     C*
062800000830     C* CALCOLA LA DATA CORRENTE
062900000830     C                   time                    wn14             14 0
063000000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063100050802     C                   movel     wn14          oracor            6 0          *ORA
063200000830     C                   z-add     wn8           g08dat
063300000830     C                   z-add     *zeros        g08inv
063400000830     C                   movel     '0'           g08err
063500000830     C                   call      'XSRDA8'
063600000830     C                   parm                    wlbda8
063700000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063800000830     C*
063900000613     C                   ENDSR
064000000613     C***
