000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050627     Ftabel00f  if   e           k disk
000700021113     FFIVABwwr  O    E             DISK    usropn
000800021113     FFIVATwwr  O    E             DISK    usropn
000900050922     Ftitvo5p   O    f  132        PRINTER usropn
001000000621     F                                     oflind(*inoa)
001100070411     F                                     infsr(*pssr)
001200050922     Ftitvo5ps  O    f  198        PRINTER usropn
001300000621     F                                     oflind(*inob)
001400070411     F                                     infsr(*pssr)
001500990908
001600000512     D*------------
001700000512     D* COMANDI
001800000512     D*------------
001900011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002000000801     D*----------------------------------------------------
002100000801     D* DICHIARAZIOINE VARIABILI DI WRK
002200000801     D*----------------------------------------------------
002300990920     D dscmz         e ds                  inz
002400990910     D psds           sds
002500990910     D  procname         *PROC
002600990920     D tivlrds       e ds                  extname(tivlr00f)
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600010202     D parccm          s              8    INZ(*blanks)
003700010202     D parmbr          s             10    INZ(*blanks)
003800010202     D paropz          s              1    INZ(*blanks)
003900010202     D chkcall         s              1    INZ(*blanks)
004000000830
004100000830     D*------------------
004200000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004300000830     D*------------------
004400000830     D WLBDA8          DS                  INZ
004500000830     D  G08DAT                 1      8  0
004600000830     D  G08INV                 9     16  0
004700000830     D  G08ERR                17     17
004800000830     D  G08TGI                18     22  0
004900050627     D*------------------
005000050627     D* DS REPERIMENTO NUMERATORE
005100050627     D*------------------
005200050627     D trul33ds      e ds                  inz
005300050627     D*------------------
005400050627     D* DS ARCHITETTURA
005500050627     D*------------------
005600050627     D kpjba         e ds                  inz
005700050627     D*------------------
005800000830     D*
005900990908
006000010201
006100010201
006200990915     C                   time                    wrkdata
006300990915     C                   time                    wrkora
006400000913     C                   reset                   rrnum
006500990921     C                   reset                   esito
006600990921     C                   reset                   wrkesito
006700000724     C*
006800000724     C* SE OCCORRE SPEDIRE IN FILIALE
006900000724     C                   if        vlrpoi <> *zeros
007000000724     C*
007100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007300000724     C     vlrpoi        chain     azorg01l
007400000724     C                   if        %found
007500000616     C                   movel(p)  CMD(1)        depcmd
007600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007700000616     C*
007800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007900011113     C                   Z-ADD     150           LENGH            15 5
008000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008100000616     C                   PARM                    depcmd
008200000616     C                   PARM                    LENGH
008300000724     C*
008400000724     C                   endif
008500000724     C                   endif
008600000616     C*
008700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008800000616     C                   movel(p)  CMD(2)        depcmd
008900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009000000616     C*
009100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009200011113     C                   Z-ADD     150           LENGH            15 5
009300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009400000616     C                   PARM                    depcmd
009500000616     C                   PARM                    LENGH
009600000616     C*
009700050922     C                   if        not %open(titvo5ps)
009800050922     C                   open      titvo5ps
009900000616     C                   except    testdett
010000000613     C                   endif
010100000613     C*
010200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010400000613     C*
010500010202     C* Effettuo la chiamata al CLLE preposto
010600040506     C                   call(e)   'TITVVTC'
010700010202     C                   parm                    parccm
010800010202     C                   parm                    parmbr
010900010202     C                   parm      '2'           paropz
011000050922     C*
011100050922     C* Chiamo in chiusura il TRUL33R
011200050922     C                   clear                   TRUL33DS
011300050922     C                   eval      I33TLA = 'C'
011400050922     C                   movel     TRUL33DS      KPJBU
011500050922     C                   call      'TRUL33R'
011600050922     C                   parm                    KPJBA
011700010202     C*
011800050922     C                   if        %open(titvo5ps)
011900000616     C                   except    findett
012000050922     C                   close     titvo5ps
012100000613     C                   endif
012200000616     C*
012300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012400011113     C                   Z-ADD     150           LENGH            15 5
012500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012600000616     C                   PARM                    CMD(3)
012700000616     C                   PARM                    LENGH
012800000616     C*
012900000801     C
013000010201     C                   seton                                        LR
013100000613
013200000613
013300000613     C*--------------------------------------------------------
013400000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013500000613     C*--------------------------------------------------------
013600000621     C     STPR          BEGSR
013700000613     C*
013800050922     C                   if        not %open(titvo5p)
013900050922     C                   open      titvo5p
014000990915     C                   endif
014100990915     C*
014200990915     C                   except    riepilogo
014300990915     C*
014400050922     C                   if        %open(titvo5p)
014500050922     C                   close     titvo5p
014600990914     C                   endif
014700990910     C*
014800000613     C                   ENDSR
014900000613     C***
015000990908
015100000801
015200910830     C*--------------------------------------------------------
015300040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
015400910830     C*--------------------------------------------------------
015500040526     C     RWFILE        BEGSR
015600990910     C*
015700990914     C                   if        not %open(tivin00r)
015800990908     C                   open      tivin00r
015900990914     C                   endif
016000021113     C                   if        not %open(fivabwwr)
016100021113     C                   open      fivabwwr
016200990914     C                   endif
016300021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016400020305     C                   exsr      prevat
016500010201     C*
016600010202     C                   if        chkcall = '0'
016700010202     C*
016800021113     C                   if        not %open(fivatwwr)
016900021113     C                   open      fivatwwr
017000010201     C                   endif
017100990910     C*
017200010201     C                   clear                   §CTROKVB          5 0
017300020305     C                   clear                   §CTROKVT          5 0
017400000801     C                   clear                   §CTRMO            5 0
017500000801     C                   clear                   §CTRNO            5 0
017600990910     C*
017700921023     C                   DO        *HIVAL
017800990913     C*
017900990915     C                   READ      tivin00r                               70
018000050627     C                   if        vindta > *blanks
018100000613     C                   add       1             rrnum
018200000801     C*
018300000801     C                   if        *in70 = *off
018400000801     C                             and
018500000801     C                             (vinflg = *blanks
018600000801     C                              or vinflg = '0'
018700000801     C                              or vinflg = '2')
018800000801     C*
018900000801     C                   clear                   vinmsg
019000000801     C                   eval      vinflg = '1'
019100050922     C*
019200050922     C* Elaboro record a record in quanto spedizioni SEMPRE mono-collo
019300050628     C                   clear                   fivab000
019400050922     C                   clear                   fivat000
019500050628     C*
019600050628     C                   exsr      impvab                                       => carico VAB
019700050922     C                   exsr      wrivab
019800050628     C                   exsr      exevat                                       => carico VAT
019900050628     C                   endif
020000000905     C*
020100000905     C                   else
020200000905     C                   eval      vinflg = '1'
020300000905     C                   endif
020400000905     C*
020500000905     C  N70              update    tivin000
020600000905     C*
020700991022     C  N70              ENDdo
020800010202     C*
020900010202     C                   endif
021000990910
021100990910     C* Se non ci sono record con errori ...
021200000710     C                   if        §ctrno = 0
021300990910     C* ... restituisco esito OK.
021400990921     C                   eval      wrkesito = '0'
021500990910     C                   else
021600010201     C                   if        §ctrokvb > 0
021700990921     C                   eval      wrkesito = '1'
021800000710     C                   else
021900000710     C                   eval      wrkesito = '2'
022000990910     C                   endif
022100000710     C                   endif
022200990910     C*
022300990914     C                   if        %open(tivin00r)
022400990908     C                   close     tivin00r
022500990914     C                   endif
022600021113     C                   if        %open(fivabwwr)
022700021113     C                   close     fivabwwr
022800990914     C                   endif
022900021113     C                   if        %open(fivatwwr)
023000021113     C                   close     fivatwwr
023100010201     C                   endif
023200990910     C*
023300010201     C                   if        §ctrokvb > 0
023400000724     C                             and vlrpoi <> *zeros
023500010202     C                   exsr      invio
023600990920     C                   endif
023700990920     C*
023800910830     C                   ENDSR
023900000613     C***
024000010305
024100010305     C*----------------------------------------------------*
024200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
024300010305     C*----------------------------------------------------*
024400020305     C     WRIVAB        BEGSR
024500010305     C*
024600021113     C                   write     fivab000                                     => scarico il VAB
024700010305     C*
024800010305     C                   ENDSR
024900050922     C*----------------------------------------------------*
025000990920
025100000801     C*----------------------------------------------------*
025200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
025300000801     C*----------------------------------------------------*
025400010201     C     INZVAR        BEGSR
025500000801     C*
025600040802     C                   Z-ADD     *zeros        Num5_0            5 0
025700040802     C                   MOVEL     '0'           FlgCAS            1
025800000801     C*
025900000801     C                   ENDSR
026000000801     C*----------------------------------------------------*
026100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
026200000801     C*----------------------------------------------------*
026300000801     C     DEFCAM        BEGSR
026400000801     C*
026500020619     C* Imposto i valori di default...
026600050922     C                   Z-ADD     0012741       VABCCM
026700050922     C                   Z-ADD     0012741       VATCCM
026800050922     C                   Z-ADD     001           VABLNP
026900050922     C                   Z-ADD     001           VATLNP
027000050922     C                   Z-ADD     001           VABCTR
027100050922     C                   Z-ADD     1             VABNCL
027200040714     C                   MOVEL     '7Q'          VABCTM
027300040823     C                   MOVEL     '1'           VABCBO
027400020619     C* ... e poi verifico se sono stati passati come parametri
027500020619     C                   IF        vlrppt > *blanks
027600040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
027700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
027800020619     C                   EXSR      CHKNUM
027900020619     C                   IF        PiInt=*on
028000020619     C                   Z-ADD     PiVal         VABCCM
028100020619     C                   Z-ADD     PiVal         VATCCM
028200020619     C                   ENDIF
028300040506     C                   ENDIF
028400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
028500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
028600020619     C                   EXSR      CHKNUM
028700020619     C                   IF        PiInt=*on
028800020619     C                   Z-ADD     PiVal         VABLNP
028900020619     C                   Z-ADD     PiVal         VATLNP
029000040506     C                   ENDIF
029100020619     C                   ENDIF
029200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
029300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
029400020619     C                   EXSR      CHKNUM
029500020619     C                   IF        PiInt=*on
029600020619     C                   Z-ADD     PiVal         VABCTR
029700040506     C                   ENDIF
029800020619     C                   ENDIF
029900020619     C                   ENDIF
030000000801     C*
030100000801     C                   ENDSR
030200000801     C*----------------------------------------------------*
030300021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
030400000801     C*----------------------------------------------------*
030500040823     C     IMPVAB        BEGSR
030600040823     C*
030700020305     C                   EXSR      INZVAR
030800020305     C                   EXSR      DEFCAM
030900010305     C*
031000000801     C                   Z-ADD     *zeros        errore            1 0
031100000830     C                   MOVEL     datcor        VABAAS
031200020305     C                   MOVEL     datcor        VATAAS
031300040526     C                   MOVE      datcor        VABMGS
031400040823     C                   MOVE(P)   vlrpoi        VABFGS
031500040823     C                   MOVE(P)   vlrpoi        VATFGS
031600050628     C*
031700050922     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
031800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
031900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032000020117     C* ==
032100050922     C                   EVAL      VABRD2=%trim(%subst(vindta:316:24))
032200050922     C                   EVAL      VABIND=%trim(%subst(vindta:341:35))
032300050922     C                   EVAL      VABLOD=%trim(%subst(vindta:400:29))
032400050922     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
032500050922     C                   EVAL      VABRMA=%trim(%subst(vindta:480:20))
032600050922     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
032700050922     C                   EVAL      VABNT2=%trim(%subst(vindta:581:33))
032800050922     C                   IF        %trim(%subst(vindta:500:2)) = 'EU'
032900050922     C                   EVAL      VABVCA='EUR'
033000050922     C                   ELSE
033100050922     C                   EVAL      VABVCA='???'
033200050922     C                   ENDIF
033300050628     C* CAP
033400050922     C                   EVAL      PiStr=%trim(%subst(vindta:391:9))
033500010201     C                   EXSR      CHKNUM
033600010201     C                   IF        PiInt=*on
033700010201     C                   Z-ADD     PiVal         Num5_0
033800040506     C                   MOVEL(P)  Num5_0        VABCAD
033900010201     C                   ELSE
034000040506     C                   ADD       1             errore
034100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034200040506     C                             + ' ' + 'VABCAD'
034300010201     C                   ENDIF
034400050922     C* RMN
034500050922     C                   EVAL      PiStr=%trim(%subst(vindta:480:20))
034600050922     C                   EXSR      CHKNUM
034700050922     C                   IF        PiInt=*on
034800050922     C                   Z-ADD     PiVal         VABRMN
034900050922     C                   ELSE
035000050922     C                   ADD       1             errore
035100050922     C                   EVAL      vinmsg = %trimr(vinmsg)
035200050922     C                             + ' ' + 'VABRMN'
035300050922     C                   ENDIF
035400040506     C* PKB
035500050922     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
035600010201     C                   EXSR      CHKNUM
035700010201     C                   IF        PiNum=*on
035800050922     C                   EVAL      PiVal = PiVal/1000                           * gestisco 3 decim.
035900050922     C                   Z-ADD     PiVal         VABPKB
036000010201     C                   ELSE
036100010201     C                   ADD       1             errore
036200010201     C                   EVAL      vinmsg = %trimr(vinmsg)
036300010201     C                             + ' ' + 'VABPKB'
036400010201     C                   ENDIF
036500050922     C* CAS
036600050922     C                   IF        %subst(vindta:502:9) <> *blanks
036700050922     C                   EVAL      FlgCAS = '1'
036800050922     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
036900050922     C                   EXSR      CHKNUM
037000050922     C                   IF        PiNum=*on
037100050922     C                   Z-ADD     PiVal         VABCAS
037200050922     C                   ELSE
037300050922     C                   ADD       1             errore
037400050922     C                   EVAL      vinmsg = %trimr(vinmsg)
037500050922     C                             + ' ' + 'VABCAS'
037600050922     C                   ENDIF
037700050922     C                   ENDIF
037800050922     C* NSP
037900050922     C* => Stacco un numeratore da AZNUM
038000050627     C                   clear                   TRUL33DS
038100050627     C                   eval      I33OPE = *zeros
038200050627     C                   eval      I33CNU = 302
038300050627     C                   eval      I33NUM = 1
038400050627     C                   movel     TRUL33DS      KPJBU
038500050627     C                   call      'TRUL33R'
038600050627     C                   parm                    KPJBA
038700050627     C                   movel     KPJBU         TRUL33DS
038800050627     C                   if        O33ERR = *zeros
038900050627     C                   z-add     O33NRF        VABNSP
039000050627     C                   z-add     O33NRF        VATNSP
039100050627     C                   else
039200050627     C                   Z-ADD     1             errore
039300050627     C                   EVAL      vinmsg = %trimr(vinmsg)
039400050922     C                             + ' ' + 'VABNSP VATNSP'
039500050628     C                   endif
039600010205     C*
039700010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
039800040802     C                   IF        FlgCAS <> '0'
039900010205     C                   EVAL      VABCBO = '4'
040000010205     C                   ELSE
040100010205     C                   EVAL      VABCBO = '1'
040200010205     C                   ENDIF
040300020305     C*
040400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
040500011113     C                   EXSR      CHKIMPDIV
040600010202     C*
040700000801     C* Ebbene...
040800000801     C                   ADD       1             §CTRMO
040900010201     C                   IF        errore <> *zeros
041000000801     C                   ADD       1             §CTRNO
041100000801     C                   EVAL      recko = vindta
041200000801     C                   EXCEPT    rigadett
041300000801     C                   EVAL      vinflg = '2'
041400000801     C                   ELSE
041500010201     C                   ADD       1             §CTROKVB
041600000801     C                   ENDIF
041700000801     C*
041800000801     C                   ENDSR
041900050628     C*----------------------------------------------------*
042000050628     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
042100050628     C*----------------------------------------------------*
042200050628     C     EXEVAT        BEGSR
042300050628     C*
042400050628     C* Valorizzo il barcode
042500050628     C                   EVAL      VATTRC='E'
042600050922     C                   EVAL      VATNOT = %trim(%subst(vindta:46:24))
042700050628     C                   exsr      wrivat                                       => scarico VAT
042800050922     C*
042900050922     C* Valorizzo il telefono destinatario
043000050922     C                   EVAL      VATTRC='B'
043100050922     C                   EVAL      VATNOT = %trim(%subst(vindta:376:13))
043200050922     C                   exsr      wrivat                                       => scarico VAT
043300050628     C*
043400050628     C                   ENDSR
043500010201     C*----------------------------------------------------*
043600040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
043700010201     C*----------------------------------------------------*
043800020305     C     WRIVAT        BEGSR
043900050628     C*
044000040802     C                   WRITE     FIVAT000
044100010201     C*
044200010201     C                   ENDSR
044300010202     C*----------------------------------------------------*
044400021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044500010202     C*----------------------------------------------------*
044600020305     C     PREVAT        BEGSR
044700010202     C*
044800021113     C* Compongo il nome del membro da dare al FIVATWWR
044900010202     C                   eval      parmbr = vlrhdl
045000010202     C                   movel     'M'           parmbr
045100050627     C                   eval      parccm = %subst(vlrKSC:2:7)
045200010202     C                   eval      paropz = '1'
045300010202     C* Effettuo la chiamata al CLLE preposto
045400040506     C                   call(e)   'TITVVTC'
045500010202     C                   parm                    parccm
045600010202     C                   parm                    parmbr
045700010202     C                   parm                    paropz
045800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045900010202     C                   if        %error
046000010202     C                   movel     '1'           chkcall
046100010202     C                   else
046200010202     C                   movel     '0'           chkcall
046300010202     C                   endif
046400010202     C*
046500010202     C                   ENDSR
046600000801     C*----------------------------------------------------*
046700000801     C*  CONTROLLO NUMERICITA' CAMPI
046800000801     C*----------------------------------------------------*
046900000801     C     CHKNUM        BEGSR
047000000801     C*
047100000801     C                   call(e)   'ISNUMERIC'
047200000801     C                   PARM                    PiStr            30
047300050922     C                   PARM      '.'           PiDecChr          1
047400000801     C                   PARM      *ZEROS        PiVal            30 9
047500000801     C                   PARM      '0'           PiInt             1
047600000801     C                   PARM      '0'           PiNum             1
047700000801     C                   IF        %error
047800000801     C                   EVAL      PiInt=*off
047900000801     C                   ENDIF
048000000801     C*
048100000801     C                   ENDSR
048200000801     C***
048300000801
048400011113
048500011113     C*----------------------------------------------------*
048600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
048700011113     C*----------------------------------------------------*
048800011113     C     CHKIMPDIV     BEGSR
048900011113     C*
049000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049100011113     C                   Z-ADD     *zeros        wrkDec            9 9
049200011113     C*
049300011113     C* Come prima cosa effettuo considerazioni sulla divisa
049400011113     C                   IF        vabIAS > *zeros
049500011113     C                   IF        vabVAS <> 'EUR'
049600011113     C                   EVAL      vabVAS =  'ITL'
049700011113     C                   ENDIF
049800011113     C                   ENDIF
049900011113     C*
050000011113     C                   IF        vabCAS > *zeros
050100011113     C                   IF        vabVCA <> 'EUR'
050200011113     C                   EVAL      vabVCA =  'ITL'
050300011113     C                   ENDIF
050400011113     C                   ENDIF
050500011113     C*
050600011113     C                   IF        vabVMD > *zeros
050700020305     C                   IF        vabVAD <> 'EUR'
050800011113     C                   EVAL      vabVAD =  'ITL'
050900011113     C                   ENDIF
051000011113     C                   ENDIF
051100011113     C*
051200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051300011113     C                   Z-ADD     vabIAS        wrkDec
051400011113     C                   IF        wrkDec > *zeros
051500011113     C                   IF        vabVAS = 'ITL'
051600011113     C                   EVAL      vabIAS = *zeros
051700011113     C                   ENDIF
051800011113     C                   ENDIF
051900011113     C*
052000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
052100011113     C                   Z-ADD     vabCAS        wrkDec
052200011113     C                   IF        wrkDec > *zeros
052300011113     C                   IF        vabVCA = 'ITL'
052400011113     C                   EVAL      vabCAS = *zeros
052500011113     C                   ENDIF
052600011113     C                   ENDIF
052700011113     C*
052800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
052900011113     C                   Z-ADD     vabVMD        wrkDec
053000011113     C                   IF        wrkDec > *zeros
053100011113     C                   IF        vabVAD = 'ITL'
053200011113     C                   EVAL      vabVMD = *zeros
053300011113     C                   ENDIF
053400011113     C                   ENDIF
053500011113     C*
053600011113     C                   ENDSR
053700011113     C***
053800011113
053900011113
054000000801
054100000801
054200990920      /TITLE Invio dei dati al punto operativo.
054300010202     C     invio         BEGSR
054400990920     C*
054500021113     C* 1° invio FIVAT
054600010201     C                   reset                   dscmz
054700010201     C                   move      vlrpoi        cmzdst
054800021113     C                   eval      cmzfld = 'FIVATWWR'
054900010201     C                   eval      cmzmbd = vlrhdl
055000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055100021009     C***                if        prmfir = *blanks
055200021113     C                   eval      cmzfla = 'FIVAT00F'
055300021113     C                   eval      cmzmba = 'FIVAT00F'
055400021009     C***                else
055500021009     C***                eval      cmzfla = prmfir
055600021009     C***                eval      cmzmba = prmfir
055700021009     C***                endif
055800010201     C                   eval      cmznrr = *zeros
055900020305     C                   move      §ctrokvt      cmznrr
056000021018     C                   eval      cmzlba = vlrfl1
056100010201     C                   call(e)   'TIS711C'
056200010201     C                   parm                    dscmz
056300010201     C                   parm      *blanks       esito
056400010205     C                   if        %error
056500010205     C                             or cmzerr = '1'
056600010205     C                             or esito  = '1'
056700010205     C                   eval      wrkesito = '3'
056800010205     C                   else
056900010201     C*
057000021113     C* 2° invio FIVAB
057100010201     C                   reset                   dscmz
057200010201     C                   move      vlrpoi        cmzdst
057300010201     C                   eval      cmzfld = vlrfou
057400010201     C                   eval      cmzmbd = vlrhdl
057500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057600021009     C***                if        prmfir = *blanks
057700021113     C                   eval      cmzfla = 'FIVAB00F'
057800021113     C                   eval      cmzmba = 'FIVAB00F'
057900021009     C***                else
058000021009     C***                eval      cmzfla = prmfir
058100021009     C***                eval      cmzmba = prmfir
058200021009     C***                endif
058300010201     C                   eval      cmznrr = *zeros
058400010201     C                   move      §ctrokvb      cmznrr
058500021018     C                   eval      cmzlba = vlrfl1
058600010201     C                   call(e)   'TIS711C'
058700010201     C                   parm                    dscmz
058800010201     C                   parm      *blanks       esito
058900010201     C                   if        %error
059000010201     C                             or cmzerr = '1'
059100010201     C                             or esito  = '1'
059200010201     C                   eval      wrkesito = '3'
059300010201     C                   endif
059400010205     C                   endif
059500990920     C*
059600000613     C                   ENDSR
059700000613     C***
059800070411
059900070411     C     *pssr         BEGSR
060000070411     C*
060100070411     C                   if        %open(tivin00r)
060200070411     C                   close     tivin00r
060300070411     C                   endif
060400070411     C                   if        %open(fivabwwr)
060500070411     C                   close     fivabwwr
060600070411     C                   endif
060700070411     C                   if        %open(fivatwwr)
060800070411     C                   close     fivatwwr
060900070411     C                   endif
061000070411     C*
061100070411     C* Effettuo la chiamata al CLLE preposto
061200070411     C                   call(e)   'TITVVTC'
061300070411     C                   parm                    parccm
061400070411     C                   parm                    parmbr
061500070411     C                   parm      '2'           paropz
061600070411     C*
061700070411     C                   eval      wrkesito = '2'
061800070411     C*
061900070411     C                   seton                                        LR
062000070411     C*
062100070411     C                   ENDSR     '*CANCL'
062200070411     C***
062300070411
062400990910
062500000613     C     *inzsr        BEGSR
062600990910     C*
062700990910     C     *entry        plist
062800990920     C                   parm                    tivlrds
062900990921     C                   parm      wrkesito      esito
063000000724     C                   parm                    prmlit
063100000710     C                   parm                    prmfir
063200000613     C*
063300000830     C* CALCOLA LA DATA CORRENTE
063400000830     C                   time                    wn14             14 0
063500000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063600000830     C                   z-add     wn8           g08dat
063700000830     C                   z-add     *zeros        g08inv
063800000830     C                   movel     '0'           g08err
063900000830     C                   call      'XSRDA8'
064000000830     C                   parm                    wlbda8
064100000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
064200000830     C*
064300000613     C                   ENDSR
064400000613     C***
064500990908
064600050922     Otitvo5p   E            riepilogo         2
064700990915     O                                              'Upload via Internet'
064800990915     O                                           +1 'Traduzione TIVIN00R -'
064900021113     O                                           +1 'FIVABWWR/FIVATWWR'
065000010201     O                                           +1 'Report testate bolle'
065100990915     O          E            riepilogo   2
065200990915     O                       wrkdata
065300990915     O                       wrkora              +1
065400990915     O                       procname            +1
065500990915     O          E            riepilogo   2
065600990915     O                                              'Cliente..................:'
065700990915     O                       VABCCM        z     +1
065800990915     O          E            riepilogo   2
065900990920     O                                              'Riferimento Strategi.....:'
066000990920     O                       vlrhdl              +1
066100990915     O          E            riepilogo   2
066200990915     O                                              'Giusti...................:'
066300010201     O                       §CTROKVB      2   +  1
066400990915     O          E            riepilogo   2
066500990915     O                                              'Sbagliati e corretti.....:'
066600971022     O                       §CTRMO        2   +  1
066700990915     O          E            riepilogo   2
066800990915     O                                              'Sbagliati e scartati.....:'
066900971022     O                       §CTRNO        2   +  1
067000000613
067100050922     Otitvo5ps  E            testdett          2
067200000613     O                                              'Upload via Internet'
067300000613     O                                           +1 'Traduzione TIVIN00R -'
067400021113     O                                           +1 'FIVABWWR/FIVATWWR'
067500010201     O                                           +1 'Report testate bolle'
067600000616     O          E            testdett    3
067700000613     O                                           +2 'N° rec'
067800000613     O                                           +3 'Anteprima contenuto'
067900000616     O          E            rigadett    2
068000000613     O                       rrnum               +2
068100000621     O                       recko               +3
068200000616     O          E            findett     2
068300000613     O                       wrkdata
068400000613     O                       wrkora              +1
068500000613     O                       procname            +1
068600000616     O          E            findett     2
068700000613     O                                              'Cliente..................:'
068800000613     O                       VABCCM        z     +1
068900000616     O          E            findett     2
069000000613     O                                              'Riferimento Strategi.....:'
069100000613     O                       vlrhdl              +1
069200000616     O          E            findett     2
069300000613     O                                              'Giusti...................:'
069400010201     O                       §CTROKVB      2   +  1
069500000616     O          E            findett     2
069600000613     O                                              'Sbagliati e corretti.....:'
069700000613     O                       §CTRMO        2   +  1
069800000616     O          E            findett     2
069900000613     O                                              'Sbagliati e scartati.....:'
070000000613     O                       §CTRNO        2   +  1
070100000512** CMD - COMANDI CL
070200050922OVRPRTF FILE(TITVO5P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
070300050922OVRPRTF FILE(TITVO5PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
070400050922DLTOVR FILE(TITVO5P TITVO5PS) LVL(*)
070500000512
070600000512
