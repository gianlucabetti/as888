000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600100412     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800110801     Ftitv2h4p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000110627     F                                     infsr(*pssr)
001100110801     Ftitv2h4ps O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300110627     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600070730     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600110801     D depspe          s             15    INZ(*blanks)
003700110801     D curspe          s             15    INZ(*blanks)
003800010202     D parccm          s              8    INZ(*blanks)
003900010202     D parmbr          s             10    INZ(*blanks)
004000010202     D paropz          s              1    INZ(*blanks)
004100010202     D chkcall         s              1    INZ(*blanks)
004200080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
004300091223     D Num5_0          s              5  0
004400020725
004500020725     D*------------------
004600020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004700020725     D*------------------
004800070530     D INPUT_DS        DS                  INZ
004900100729     D  VINDTA                     2048
005000100729     D  VINFLG                        1
005100110801     D  VINSPE                       15
005200100729     D  VINRRN                        8  0
005300020725     D*
005400081113
005500091223     D*------------------
005600091223     D* DS REPERIMENTO NUMERATORE
005700091223     D*------------------
005800100309     D trul33ds      e ds                  inz
005900091223     D*------------------
006000091223     D* DS ARCHITETTURA
006100091223     D*------------------
006200091223     D kpjba         e ds                  inz
006300091223
006400101119
006500101119     D*-------------------
006600101119     D* COSTANTI
006700101119     D*-------------------
006800101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
006900101119     D                                     èéù')
007000101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
007100101119     D                                     EEU')
007200081113
007300081113     D*------------------
007400081113     D* LINKING A DEFINIZIONI ESTERNE
007500081113     D*------------------
007600081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007700090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007800081113
007900990908
008000010201
008100010201
008200080117     C*
008300080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008400080117     C
008500080117     C/EXEC SQL
008600080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008700080117     C/END-EXEC
008800080117     C
008900990915     C                   time                    wrkdata
009000990915     C                   time                    wrkora
009100000913     C                   reset                   rrnum
009200990921     C                   reset                   esito
009300990921     C                   reset                   wrkesito
009400000724     C*
009500000724     C* SE OCCORRE SPEDIRE IN FILIALE
009600110411     C                   if        vlrpoi <> *zeros AND vlrpoi <> 999
009700000724     C*
009800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010000000724     C     vlrpoi        chain     azorg01l
010100000724     C                   if        %found
010200000616     C                   movel(p)  CMD(1)        depcmd
010300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010400000616     C*
010500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010600110627     C                   Z-ADD     150           LENGH            15 5
010700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010800000616     C                   PARM                    depcmd
010900000616     C                   PARM                    LENGH
011000000724     C*
011100000724     C                   endif
011200000724     C                   endif
011300000616     C*
011400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011500000616     C                   movel(p)  CMD(2)        depcmd
011600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011700000616     C*
011800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011900110627     C                   Z-ADD     150           LENGH            15 5
012000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012100000616     C                   PARM                    depcmd
012200000616     C                   PARM                    LENGH
012300000616     C*
012400110801     C                   if        not %open(titv2h4ps)
012500110801     C                   open      titv2h4ps
012600000616     C                   except    testdett
012700000613     C                   endif
012800000613     C*
012900100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
013000070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
013100110411     C                   if        vlrpoi <> *zeros AND vlrpoi <> 999
013200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
013300110411     C                   endif
013400000613     C*
013500010202     C* Effettuo la chiamata al CLLE preposto
013600100525     C  N51              call(e)   'TITVVTC'
013700100525     C                   parm                    parccm
013800100525     C                   parm                    parmbr
013900100525     C                   parm      '2'           paropz
014000100525     C   51              call(e)   'TITVVBC'
014100100525     C                   parm                    parccm
014200100525     C                   parm                    parmbr
014300100525     C                   parm      '2'           paropz
014400070730     C*
014500070730     C* Effettuo lancio TISI95 solo x chiusura
014600070730     C                   CLEAR                   TISI95DS
014700070730     C                   EVAL      I95TLA = 'C'
014800070730     C                   CALL      'TISI95R'
014900070730     C                   PARM                    TISI95DS
015000010202     C*
015100110801     C                   if        %open(titv2h4ps)
015200000616     C                   except    findett
015300110801     C                   close     titv2h4ps
015400000613     C                   endif
015500000616     C*
015600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015700110411     C                   if        vlrpoi <> *zeros AND vlrpoi <> 999
015800110627     C                   Z-ADD     100           LENGH            15 5
015900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016000000616     C                   PARM                    CMD(3)
016100000616     C                   PARM                    LENGH
016200110411     C                   endif
016300110411     C*
016400110411     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
016500110627     C                   Z-ADD     100           LENGH            15 5
016600110411     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016700110411     C                   PARM                    CMD(4)
016800110411     C                   PARM                    LENGH
016900000616     C*
017000000801     C
017100010201     C                   seton                                        LR
017200000613
017300100310
017400100310
017500000613     C*--------------------------------------------------------
017600000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
017700000613     C*--------------------------------------------------------
017800000621     C     STPR          BEGSR
017900000613     C*
018000110801     C                   if        not %open(titv2h4p)
018100110801     C                   open      titv2h4p
018200990915     C                   endif
018300990915     C*
018400110411     C                   except    riepilogo
018500990915     C*
018600110801     C                   if        %open(titv2h4p)
018700110801     C                   close     titv2h4p
018800990914     C                   endif
018900990910     C*
019000000613     C                   ENDSR
019100000613     C***
019200990908
019300000801
019400910830     C*--------------------------------------------------------
019500070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
019600910830     C*--------------------------------------------------------
019700070530     C     RWFILE        BEGSR
019800990910     C*
019900990914     C                   if        not %open(tivin00r)
020000990908     C                   open      tivin00r
020100990914     C                   endif
020200070530     C*
020300100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
020400100525     C                   exsr      prevasx
020500010201     C*
020600010202     C                   if        chkcall = '0'
020700010202     C*
020800100525     C                   if        not %open(fivabwwr)
020900100525     C                   open      fivabwwr
021000100525     C                   endif
021100100525     C*
021200021113     C                   if        not %open(fivatwwr)
021300021113     C                   open      fivatwwr
021400010201     C                   endif
021500080117     C*
021600080117     C                   EXSR      INZVAR
021700080117     C                   EXSR      DEFCAM
021800990910     C*
021900010201     C                   clear                   §CTROKVB          5 0
022000020305     C                   clear                   §CTROKVT          5 0
022100000801     C                   clear                   §CTRMO            5 0
022200000801     C                   clear                   §CTRNO            5 0
022300990910     C*
022400020725     C/EXEC SQL
022500020725     C+ declare C1 cursor for select
022600110801     C+ vindta, vinflg, substr(vindta, 21, 15) as sped, rrn(tivin00r)
022700060223     C+ from tivin00r
022800110801     C+ order by sped, substr(vindta, 21, 15) desc
022900020725     C+ for read only
023000020725     C/END-EXEC
023100020725     C
023200020725     C/EXEC SQL
023300020725     C+ open C1
023400020725     C/END-EXEC
023500020725     C
023600020725     C/EXEC SQL
023700070530     C+ Fetch C1 into :INPUT_DS
023800020725     C/END-EXEC
023900020725     C*
024000020725     C                   dow       sqlcod = *zeros
024100990913     C*
024200100310     C                   if        vindta > *blanks
024300000613     C                   add       1             rrnum
024400000801     C*
024500020725     C                   if        vinflg = *blanks
024600020725     C                             or vinflg = '0'
024700020725     C                             or vinflg = '2'
024800000801     C*
024900020725     C                   clear                   x_vinmsg
025000020725     C                   eval      x_vinflg = '1'
025100101119     C*
025200101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
025300101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
025400010305     C*
025500010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
025600110801     C                   EVAL      curspe=%trim(%subst(vindta:21:15))
025700010305     C*
025800100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
025900080923     C                   if        *in50 = *off
026000101119     C                   exsr      inzvar
026100101119     C                   exsr      defcam
026200071003     C                   exsr      impvab
026300100525     C                   exsr      wrivab
026400100127     C                   exsr      wrivat_a                                     => carico VAT
026500071003     C                   exsr      wrivat_b                                     => carico VAT
026600100525     C   51              exsr      wrivat_e                                     => carico VAT
026700071003     C                   else
026800080923     C*
026900071009     C                   if        wDISK = 'D'
027000091223     C                   exsr      repNSP
027100071009     C                   exsr      impvab
027200100412     C                   exsr      wrivab
027300100127     C                   exsr      wrivat_a                                     => carico VAT
027400071009     C                   exsr      wrivat_b                                     => carico VAT
027500071009     C                   exsr      wrivat_e                                     => carico VAT
027600071009     C                   else
027700071009     C*
027800010305     C                   if        depspe = *blanks                             => 1° giro
027900010305     C                   eval      depspe = curspe                              => memorizz. spediz
028000080117     C                   clear                   tivinds
028100091223     C                   exsr      repNSP
028200020305     C                   exsr      impvab
028300100127     C                   exsr      wrivat_a                                     => carico VAT
028400071003     C                   exsr      wrivat_b                                     => carico VAT
028500071003     C   50              exsr      wrivat_e                                     => carico VAT
028600010305     C                   else
028700020725     C                   if        depspe <> curspe                             => rottura di spediz
028800010305     C                   eval      depspe = curspe                              => memorizz. spediz
028900100412     C                   exsr      wrivab
029000080117     C                   clear                   tivinds
029100091223     C                   exsr      repNSP
029200020305     C                   exsr      impvab
029300100127     C                   exsr      wrivat_a                                     => carico VAT
029400071003     C                   exsr      wrivat_b                                     => carico VAT
029500071003     C   50              exsr      wrivat_e                                     => carico VAT
029600020305     C                   else                                                   => x stessa spediz
029700100401     C*
029800100401     C* Se nn richiesta esclusione spedizioni "duplicate"
029900100928     C                   select
030000100928     C                   when      wDUPREC = ' '
030100020305     C                   exsr      impvab
030200100127     C                   exsr      wrivat_a                                     => carico VAT
030300071003     C                   exsr      wrivat_b                                     => carico VAT
030400071003     C   50              exsr      wrivat_e                                     => carico VAT
030500100928     C                   when      wDUPREC = 'C'
030600100928     C   50              exsr      wrivat_e                                     => carico VAT
030700100928     C                   endsl
030800010305     C                   endif
030900010305     C                   endif
031000010305     C                   endif
031100071003     C                   endif
031200110201     C*
031300071009     C                   endif
031400000905     C*
031500000905     C                   else
031600020725     C                   eval      x_vinflg = '1'
031700000905     C                   endif
031800000905     C*
031900020725     C     VINRRN        chain     tivin000
032000020725     C                   if        %found(tivin00r)
032100020725     C                   eval      y_vinflg = x_vinflg
032200020725     C                   eval      y_vinmsg = x_vinmsg
032300020725     C                   update    tivin000
032400020725     C                   endif
032500020725     C*
032600020725     C/EXEC SQL
032700070530     C+ Fetch C1 into :INPUT_DS
032800020725     C/END-EXEC
032900020725     C*
033000020725     C                   enddo
033100020725     C*
033200020725     C/EXEC SQL
033300020725     C+ close C1
033400020725     C/END-EXEC
033500000905     C*
033600020305     C* Scarico i VAB rimasti "in sospeso"
033700071009     C                   if        wDISK = 'C'
033800100412     C                   exsr      wrivab
033900071009     C                   endif
034000010202     C*
034100010202     C                   endif
034200990910
034300990910     C* Se non ci sono record con errori ...
034400000710     C                   if        §ctrno = 0
034500990910     C* ... restituisco esito OK.
034600990921     C                   eval      wrkesito = '0'
034700990910     C                   else
034800100525     C                   if        §ctrokvb > 0 OR
034900100525     C                             §ctrokvt > 0
035000990921     C                   eval      wrkesito = '1'
035100000710     C                   else
035200000710     C                   eval      wrkesito = '2'
035300990910     C                   endif
035400000710     C                   endif
035500990910     C*
035600990914     C                   if        %open(tivin00r)
035700990908     C                   close     tivin00r
035800990914     C                   endif
035900021113     C                   if        %open(fivabwwr)
036000021113     C                   close     fivabwwr
036100990914     C                   endif
036200021113     C                   if        %open(fivatwwr)
036300021113     C                   close     fivatwwr
036400010201     C                   endif
036500990910     C*
036600100525     C                   if        §ctrokvb > 0 OR
036700100525     C                             §ctrokvt > 0
036800000724     C                             and vlrpoi <> *zeros
036900010202     C                   exsr      invio
037000990920     C                   endif
037100990920     C*
037200910830     C                   ENDSR
037300000613     C***
037400100412
037500100412
037600010305
037700010305     C*----------------------------------------------------*
037800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
037900010305     C*----------------------------------------------------*
038000020305     C     WRIVAB        BEGSR
038100100412     C*
038200100412     C* Gestisco l'eventuale rottura x numero spedizione
038300100412     C                   if        VABRMA = *blanks
038400100412     C                   movel(P)  VABRMN        VABRMA
038500100412     C                   endif
038600100412     C*
038700100525     C                   if        *in51 = *off and *in31 = *off
038800100525     C                   write     fivab000                                     => scarico il VAB
038900100525     C                   endif
039000010305     C*
039100010305     C                   ENDSR
039200990920
039300000801     C*----------------------------------------------------*
039400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
039500000801     C*----------------------------------------------------*
039600010201     C     INZVAR        BEGSR
039700000801     C*
039800010201     C                   Z-ADD     *zeros        Num5_0
039900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
040000020725     C                   MOVEL     '0'           FlgCAS            1
040100000801     C*
040200000801     C                   ENDSR
040300000801     C*----------------------------------------------------*
040400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
040500000801     C*----------------------------------------------------*
040600020904     C     DEFCAM        BEGSR
040700080923     C*
040800100928     C                   SETOFF                                       505160
040900000801     C*
041000021113     C                   CLEAR                   FIVAB000
041100021113     C                   CLEAR                   FIVAT000
041200070730     C* Imposto i valori di default...
041300110805     C                   EVAL      VABCCM = 1332568
041400110805     C                   EVAL      VATCCM = 1332568
041500110805     C                   EVAL      VABLNP = 059
041600110805     C                   EVAL      VATLNP = 059
041700110627     C                   EVAL      VABCTR = 000
041800100525     C                   EVAL      VABCBO = '1'
041900070222     C* ... e poi verifico se sono stati passati come parametri
042000070222     C                   IF        vlrppt > *blanks
042100100525     C*
042200070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
042300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
042400070222     C                   EXSR      CHKNUM
042500070222     C                   IF        PiInt=*on
042600070222     C                   Z-ADD     PiVal         VABCCM
042700070222     C                   Z-ADD     PiVal         VATCCM
042800070222     C                   ENDIF
042900070222     C                   ENDIF
043000070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
043100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
043200070222     C                   EXSR      CHKNUM
043300070222     C                   IF        PiInt=*on
043400070222     C                   Z-ADD     PiVal         VABLNP
043500070222     C                   Z-ADD     PiVal         VATLNP
043600070222     C                   ENDIF
043700070222     C                   ENDIF
043800070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
043900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
044000070222     C                   EXSR      CHKNUM
044100070222     C                   IF        PiInt=*on
044200070222     C                   Z-ADD     PiVal         VABCTR
044300070222     C                   ENDIF
044400070928     C                   ENDIF
044500071009     C                   MOVEL     *blanks       wDISK             1
044600071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
044700071009     C                   IF        wDISK <> *blanks
044800070928     C                   SETON                                        50
044900070222     C                   ENDIF
045000100525     C                   IF        wDISK  = 'T'
045100100525     C                   SETOFF                                       50
045200100525     C                   SETON                                        51
045300100525     C                   ENDIF
045400100401     C                   MOVEL     *blanks       wDUPREC           1
045500100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
045600100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
045700100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
045800100525     C                   ENDIF
045900100525     C*
046000070222     C                   ENDIF
046100071009     C*
046200071009     C   50              EVAL      VABCTM = '7Q'
046300000801     C*
046400000801     C                   ENDSR
046500091223     C*----------------------------------------------------*
046600091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
046700091223     C*----------------------------------------------------*
046800091223     C     REPNSP        BEGSR
046900091223     C*
047000091223     C                   EXSR      INZVAR
047100091223     C                   EXSR      DEFCAM
047200100928     C*
047300100928     C                   SETON                                        60
047400091223     C*
047500091223     C* NSP => Stacco un numeratore da AZNUM
047600091223     C                   clear                   TRUL33DS
047700091223     C                   eval      I33OPE = *zeros
047800091223     C                   eval      I33CNU = 302
047900091223     C                   eval      I33NUM = 1
048000091223     C                   movel     TRUL33DS      KPJBU
048100091223     C                   call      'TRUL33R'
048200091223     C                   parm                    KPJBA
048300091223     C                   movel     KPJBU         TRUL33DS
048400091223     C                   if        O33ERR = *zeros
048500091223     C                   z-add     O33NRF        VABNSP
048600091223     C                   z-add     O33NRF        VATNSP
048700091223     C                   else
048800091223     C                   SETON                                        31
048900091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049000091223     C                             + ' ' + 'VABNSP VATNSP'
049100091223     C                   endif
049200091223     C*
049300091223     C                   ENDSR
049400000801     C*----------------------------------------------------*
049500021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
049600000801     C*----------------------------------------------------*
049700100730     C     IMPVAB        BEGSR
049800070530     C*
049900070530     C                   SETOFF                                       3132
050000070928     C*
050100070928     C                   MOVE(P)   vlrpoi        VABFGS
050200070928     C                   MOVE(P)   vlrpoi        VATFGS
050300070928     C*
050400070928     C                   MOVEL     datcor        VABAAS
050500070928     C                   MOVEL     datcor        VATAAS
050600070928     C                   MOVE      datcor        VABMGS
050700100729     C*
050800100729     C* Reperimento campi ALFA
050900110801     C                   EVAL      VABRSD=%trim(%subst(vindta:200:35))
051000110801     C                   EVAL      VABRD2=%trim(%subst(vindta:235:20))
051100110801     C                   EVAL      VABRMA=%trim(%subst(vindta:21:15))
051200110801     C                   EVAL      VABIND=%trim(%subst(vindta:270:35))
051300110801     C                   EVAL      VABLOD=%trim(%subst(vindta:305:35)) +
051400110801     C                                    %trim(%subst(vindta:340:20))
051500110801     C                   EVAL      VABCAD=%trim(%subst(vindta:360:7))
051600110801     C                   EVAL      VABPRD=%trim(%subst(vindta:367:2))
051700110801     C                   EVAL      VABNZD=%trim(%subst(vindta:369:4))
051800110805     C                   IF        VABNZD='IT'
051900110805     C                   EVAL      VABNZD=*blanks
052000110805     C                   ENDIF
052100110801     C                   EVAL      VABVCA=%trim(%subst(vindta:373:4))
052200110801     C                   EVAL      VABVAD=%trim(%subst(vindta:387:4))
052300110801     C                   EVAL      VABVAS=%trim(%subst(vindta:401:4))
052400110801     C                   MOVEL     *blanks       wNOTE           120
052500110801     C                   EVAL      wNOTE=%trim(%subst(vindta:425:60))
052600110801     C                   EVAL      wNOTE=wNOTE+' '+%trim(%subst(vindta:485:60))
052700110801     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
052800110801     C                   EVAL      VABNT2=%subst(wNOTE:36:35)
052900110801     C*
053000110801     C                   SELECT
053100110801     C                   WHEN      %subst(vindta:105:1) = '1'
053200110801     C                   EVAL      VABTC1='A'
053300110801     C                   WHEN      %subst(vindta:104:1) = '1'
053400110801     C                   EVAL      VABTC2='S'
053500110801     C                   ENDSL
053600110801     C*
053700110801     C                   SELECT
053800110801     C                   WHEN      %subst(vindta:146:3) = 'DOM'
053900110801     C                   EVAL      VABGC1='7'
054000110801     C                   WHEN      %subst(vindta:146:3) = 'DO1'
054100110801     C                   EVAL      VABGC1='7M'
054200110801     C                   WHEN      %subst(vindta:146:3) = 'DO2'
054300110801     C                   EVAL      VABGC1='7P'
054400110801     C                   WHEN      %subst(vindta:146:3) = 'LUN'
054500110801     C                   EVAL      VABGC1='1'
054600110801     C                   WHEN      %subst(vindta:146:3) = 'LU1'
054700110801     C                   EVAL      VABGC1='1M'
054800110801     C                   WHEN      %subst(vindta:146:3) = 'LU2'
054900110801     C                   EVAL      VABGC1='1P'
055000110801     C                   WHEN      %subst(vindta:146:3) = 'MAR'
055100110801     C                   EVAL      VABGC1='2'
055200110801     C                   WHEN      %subst(vindta:146:3) = 'MA1'
055300110801     C                   EVAL      VABGC1='2M'
055400110801     C                   WHEN      %subst(vindta:146:3) = 'MA2'
055500110801     C                   EVAL      VABGC1='2P'
055600110801     C                   WHEN      %subst(vindta:146:3) = 'MER'
055700110801     C                   EVAL      VABGC1='3'
055800110801     C                   WHEN      %subst(vindta:146:3) = 'ME1'
055900110801     C                   EVAL      VABGC1='3M'
056000110801     C                   WHEN      %subst(vindta:146:3) = 'ME2'
056100110801     C                   EVAL      VABGC1='3P'
056200110801     C                   WHEN      %subst(vindta:146:3) = 'GIO'
056300110801     C                   EVAL      VABGC1='4'
056400110801     C                   WHEN      %subst(vindta:146:3) = 'GI1'
056500110801     C                   EVAL      VABGC1='4M'
056600110801     C                   WHEN      %subst(vindta:146:3) = 'GI2'
056700110801     C                   EVAL      VABGC1='4P'
056800110801     C                   WHEN      %subst(vindta:146:3) = 'VER'
056900110801     C                   EVAL      VABGC1='5'
057000110801     C                   WHEN      %subst(vindta:146:3) = 'VE1'
057100110801     C                   EVAL      VABGC1='5M'
057200110801     C                   WHEN      %subst(vindta:146:3) = 'VE2'
057300110801     C                   EVAL      VABGC1='5P'
057400110801     C                   WHEN      %subst(vindta:146:3) = 'SAB'
057500110801     C                   EVAL      VABGC1='6'
057600110801     C                   WHEN      %subst(vindta:146:3) = 'SA1'
057700110801     C                   EVAL      VABGC1='6M'
057800110801     C                   WHEN      %subst(vindta:146:3) = 'SA2'
057900110801     C                   EVAL      VABGC1='6P'
058000110801     C                   ENDSL
058100100729     C*
058200100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
058300100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
058400100729     C     '@':'A'       XLATE     VABRSD        VABRSD
058500100729     C* ==
058600100729     C*
058700100729     C* Reperimento campi NUMERICI
058800100729     C                   MOVEL     DATCOR        VABAAS
058900100729     C                   MOVE      DATCOR        VABMGS
059000110408     C*
059100110127     C* NSP / RMN
059200101119     C                   IF        *IN51 = *off
059300110801     C                   EVAL      PiStr=%subst(vindta:29:7)
059400100730     C                   EXSR      CHKNUM
059500100730     C                   IF        PiInt=*on
059600100730     C                   Z-ADD     PiVal         VABRMN
059700100928     C  N60              Z-ADD     PiVal         VABNSP
059800100928     C  N60              Z-ADD     PiVal         VATNSP
059900100730     C                   ELSE
060000100730     C                   SETON                                        32
060100100730     C                   Z-ADD     1             VABRMN
060200100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
060300101119     C                             + ' ' + 'VABNSP VABRMN VATNSP'
060400100730     C                   ENDIF
060500101119     C*
060600101119     C                   ELSE
060700101119     C*
060800110627     C                   EVAL      PiStr=%subst(vindta:223:5)
060900101119     C                   EXSR      CHKNUM
061000101119     C                   IF        PiInt=*on
061100101119     C  N60              Z-ADD     PiVal         VATNSP
061200101119     C                   ELSE
061300101119     C                   SETON                                        32
061400101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
061500101119     C                             + ' ' + 'VATNSP'
061600101119     C                   ENDIF
061700101119     C                   ENDIF
061800110802     C* DCR
061900110802     C                   SELECT
062000110802     C                   WHEN      %subst(vindta:113:1) = '1'
062100110802     C                   EVAL      VABDCR=%dec(%subst(vindta:114:4)+
062200110802     C                                         %subst(vindta:119:2)+
062300110802     C                                         %subst(vindta:122:2):8:0)
062400110802     C                   WHEN      %subst(vindta:113:1) = '2'
062500110802     C                   EVAL      VABDCR=%dec(%subst(vindta:124:4)+
062600110802     C                                         %subst(vindta:129:2)+
062700110802     C                                         %subst(vindta:132:2):8:0)
062800110802     C                   EVAL      VABTCR='P'
062900110802     C                   WHEN      %subst(vindta:113:1) = '3'
063000110802     C                   EVAL      VABDCR=%dec(%subst(vindta:114:4)+
063100110802     C                                         %subst(vindta:119:2)+
063200110802     C                                         %subst(vindta:122:2):8:0)
063300110802     C                   EVAL      VABTCR='D'
063400110802     C                   WHEN      %subst(vindta:113:1) = '4'
063500110802     C                   EVAL      VABDCR=%dec(%subst(vindta:114:4)+
063600110802     C                                         %subst(vindta:119:2)+
063700110802     C                                         %subst(vindta:122:2):8:0)
063800110802     C                   ENDSL
063900110801     C* NCL
064000110801     C                   EVAL      PiStr=%subst(vindta:78:5)
064100110801     C                   EXSR      CHKNUM
064200110801     C                   IF        PiInt=*on
064300110801     C                   Z-ADD     PiVal         VABNCL
064400110801     C                   ELSE
064500110801     C                   SETON                                        32
064600110801     C                   Z-ADD     *zeros        VABNCL
064700110801     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
064800110801     C                             + ' ' + 'VABNCL'
064900110801     C                   ENDIF
065000110801     C                   IF        %trim(%subst(vindta:83:5)) <> *all'0'
065100110801     C                   EVAL      PiStr=%subst(vindta:83:5)
065200110801     C                   EXSR      CHKNUM
065300110801     C                   IF        PiInt=*on
065400110801     C                   ADD       PiVal         VABNCL
065500110801     C                   ELSE
065600110801     C  N51              SETON                                        32
065700110801     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
065800110801     C                             + ' ' + 'VABNCL'
065900110801     C                   ENDIF
066000110801     C                   ENDIF
066100100729     C* PKB
066200110801     C                   EVAL      PiStr=%trim(%subst(vindta:89:7))
066300100729     C                   EXSR      CHKNUM
066400100729     C                   IF        PiNum=*on
066500110801     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
066600110627     C                   ADD(H)    PiVal         VABPKB
066700100729     C                   ELSE
066800101119     C  N51              SETON                                        32
066900101119     C  N51              Z-ADD     *zeros        VABPKB
067000101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
067100100729     C                             + ' ' + 'VABPKB'
067200100729     C                   ENDIF
067300110627     C* VLB
067400110801     C                   IF        %trim(%subst(vindta:96:7)) <> *all'0' AND
067500110801     C                             %trim(%subst(vindta:96:7)) <> *blanks
067600110801     C                   EVAL      PiStr=%trim(%subst(vindta:96:7))
067700110627     C                   EXSR      CHKNUM
067800110627     C                   IF        PiNum=*on
067900110801     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
068000110627     C                   ADD(H)    PiVal         VABVLB
068100110627     C                   ELSE
068200110627     C  N51              SETON                                        32
068300110627     C  N51              Z-ADD     *zeros        VABVLB
068400110627     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
068500110627     C                             + ' ' + 'VABVLB'
068600110627     C                   ENDIF
068700110627     C                   ENDIF
068800110801     C* CAS
068900110801     C                   IF        %trim(%subst(vindta:377:10)) <> *all'0' AND
069000110801     C                             %trim(%subst(vindta:377:10)) <> *blanks
069100110801     C                   EVAL      PiStr=%trim(%subst(vindta:377:10))
069200110801     C                   EXSR      CHKNUM
069300110801     C                   IF        PiNum=*on
069400110801     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
069500110801     C                   ADD(H)    PiVal         VABCAS
069600110801     C                   ELSE
069700110801     C  N51              SETON                                        32
069800110801     C  N51              Z-ADD     *zeros        VABCAS
069900110801     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
070000110801     C                             + ' ' + 'VABCAS'
070100110801     C                   ENDIF
070200110801     C                   ENDIF
070300110801     C* VMD
070400110801     C                   IF        %trim(%subst(vindta:391:10)) <> *all'0' AND
070500110801     C                             %trim(%subst(vindta:391:10)) <> *blanks
070600110801     C                   EVAL      PiStr=%trim(%subst(vindta:391:10))
070700110801     C                   EXSR      CHKNUM
070800110801     C                   IF        PiNum=*on
070900110801     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
071000110801     C                   ADD(H)    PiVal         VABVMD
071100110801     C                   ELSE
071200110801     C  N51              SETON                                        32
071300110801     C  N51              Z-ADD     *zeros        VABVMD
071400110801     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
071500110801     C                             + ' ' + 'VABVMD'
071600110801     C                   ENDIF
071700110801     C                   ENDIF
071800110801     C* IAS
071900110801     C                   IF        %trim(%subst(vindta:405:10)) <> *all'0' AND
072000110801     C                             %trim(%subst(vindta:405:10)) <> *blanks
072100110801     C                   EVAL      PiStr=%trim(%subst(vindta:405:10))
072200110801     C                   EXSR      CHKNUM
072300110801     C                   IF        PiNum=*on
072400110801     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
072500110801     C                   ADD(H)    PiVal         VABIAS
072600110801     C                   ELSE
072700110801     C  N51              SETON                                        32
072800110801     C  N51              Z-ADD     *zeros        VABIAS
072900110801     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
073000110801     C                             + ' ' + 'VABIAS'
073100110801     C                   ENDIF
073200110801     C                   ENDIF
073300100729     C*
073400100729     C* Se provincia nn valorizzata la reperisco
073500100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
073600100729     C                   IF        VABNZD  = *blanks AND
073700100729     C                             VABPRD  = *blanks AND
073800100729     C                             VABCAD <> *blanks
073900100729     C                   CLEAR                   TISI95DS
074000100729     C                   EVAL      I95TCN = '3'
074100100729     C                   Z-ADD     datcor        I95DAT
074200100729     C                   EVAL      I95NAR = VABNZD
074300100729     C                   EVAL      I95CAP = VABCAD
074400100729     C                   EVAL      I95LOC = VABLOD
074500100729     C                   CALL      'TISI95R'
074600100729     C                   PARM                    TISI95DS
074700100729     C                   EVAL      VABPRD = O95PRV
074800100729     C                   ENDIF
074900100729     C*
075000100729     C* Considerazioni finali su CBO/CAS
075100100729     C                   IF        FlgCAS = '1'
075200100729     C                   IF        VABCBO = '1'
075300100729     C                   EVAL      VABCBO = '4'
075400100729     C                   ENDIF
075500100729     C                   IF        VABCBO = '2'
075600100729     C                   EVAL      VABCBO = '6'
075700100729     C                   ENDIF
075800100729     C                   ENDIF
075900010202     C*
076000000801     C* Ebbene...
076100000801     C                   ADD       1             §CTRMO
076200070530     C                   IF        *in31 <> *zeros OR
076300070530     C                             *in32 <> *zeros
076400000801     C                   ADD       1             §CTRNO
076500000801     C                   EVAL      recko = vindta
076600000801     C                   EXCEPT    rigadett
076700020725     C                   EVAL      x_vinflg = '2'
076800000801     C                   ELSE
076900010201     C                   ADD       1             §CTROKVB
077000000801     C                   ENDIF
077100000801     C*
077200000801     C                   ENDSR
077300100127     C*----------------------------------------------------*
077400100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
077500100127     C*----------------------------------------------------*
077600100127     C     WRIVAT_A      BEGSR
077700100127     C*
077800100127     C* Valorizzo l buffer di scrittura del FIVAT
077900100127     C* - TIPO RECORD "A"
078000101119     C***                eval      VATTRC = 'A'
078100101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
078200101119     C***                exsr      wrivat
078300100127     C*
078400100127     C                   ENDSR
078500010201     C*----------------------------------------------------*
078600071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
078700010201     C*----------------------------------------------------*
078800071003     C     WRIVAT_B      BEGSR
078900010201     C*
079000021113     C* Valorizzo l buffer di scrittura del FIVAT
079100070928     C* - TIPO RECORD "B"
079200110627     C***                eval      VATTRC = 'B'
079300110627     C***                eval      VATNOT = %trim(%subst(vindta:206:15))
079400110627     C***                exsr      wrivat
079500010201     C*
079600010201     C                   ENDSR
079700071003     C*----------------------------------------------------*
079800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
079900071003     C*----------------------------------------------------*
080000071003     C     WRIVAT_E      BEGSR
080100071003     C*
080200071003     C* Valorizzo l buffer di scrittura del FIVAT
080300071003     C* - TIPO RECORD "E"
080400100729     C*
080500100928     C                   eval      VATTRC = 'E'
080600110627     C                   eval      VATNOT = %trim(%subst(vindta:415:11))
080700100928     C                   exsr      wrivat
080800071003     C*
080900071003     C                   ENDSR
081000100127     C*----------------------------------------------------*
081100100127     C*  SCARICO BUFFER FIVAT
081200100127     C*----------------------------------------------------*
081300100127     C     WRIVAT        BEGSR
081400100127     C*
081500100127     C                   if        vatNOT <> *blanks
081600100127     C                   ADD       1             §CTROKVT
081700100127     C                   write     FIVAT000
081800100127     C                   endif
081900100127     C*
082000100127     C                   ENDSR
082100020904
082200020904
082300010202     C*----------------------------------------------------*
082400100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
082500010202     C*----------------------------------------------------*
082600100525     C     PREVASX       BEGSR
082700100525     C*
082800021113     C* Compongo il nome del membro da dare al FIVATWWR
082900010202     C                   eval      parmbr = vlrhdl
083000010202     C                   movel     'M'           parmbr
083100070530     C                   eval      parccm = %subst(vlrKSC:2:7)
083200010202     C                   eval      paropz = '1'
083300100525     C*
083400010202     C* Effettuo la chiamata al CLLE preposto
083500100525     C  N51              call(e)   'TITVVTC'
083600010202     C                   parm                    parccm
083700010202     C                   parm                    parmbr
083800010202     C                   parm                    paropz
083900100525     C   51              call(e)   'TITVVBC'
084000100525     C                   parm                    parccm
084100100525     C                   parm                    parmbr
084200100525     C                   parm                    paropz
084300100525     C*
084400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
084500010202     C                   if        %error
084600010202     C                   movel     '1'           chkcall
084700010202     C                   else
084800010202     C                   movel     '0'           chkcall
084900010202     C                   endif
085000010202     C*
085100010202     C                   ENDSR
085200000801     C*----------------------------------------------------*
085300000801     C*  CONTROLLO NUMERICITA' CAMPI
085400000801     C*----------------------------------------------------*
085500000801     C     CHKNUM        BEGSR
085600081113     C*
085700081113     C                   IF        PiDecChr = *blanks
085800110408     C                   EVAL      PiDecChr = '.'
085900081113     C                   ENDIF
086000091223     C*
086100081113     C                   callp(e)  UBISNUM_Check(PiStr
086200081113     C                                          :PiDecChr
086300081113     C                                          :PiVal
086400081113     C                                          :PiNum
086500081113     C                                          :PiInt)
086600081113     C*
086700000801     C                   IF        %error
086800000801     C                   EVAL      PiInt=*off
086900000801     C                   ENDIF
087000000801     C*
087100000801     C                   ENDSR
087200000801     C***
087300011113
087400011113
087500000801
087600000801
087700990920      /TITLE Invio dei dati al punto operativo.
087800010202     C     invio         BEGSR
087900990920     C*
088000021113     C* 1° invio FIVAT
088100010201     C                   reset                   dscmz
088200110411     C                   move      vatfgs        cmzdst
088300021113     C                   eval      cmzfld = 'FIVATWWR'
088400010201     C                   eval      cmzmbd = vlrhdl
088500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
088600021009     C***                if        prmfir = *blanks
088700021113     C                   eval      cmzfla = 'FIVAT00F'
088800021113     C                   eval      cmzmba = 'FIVAT00F'
088900021009     C***                else
089000021009     C***                eval      cmzfla = prmfir
089100021009     C***                eval      cmzmba = prmfir
089200021009     C***                endif
089300010201     C                   eval      cmznrr = *zeros
089400020305     C                   move      §ctrokvt      cmznrr
089500021018     C                   eval      cmzlba = vlrfl1
089600010201     C                   call(e)   'TIS711C'
089700010201     C                   parm                    dscmz
089800010201     C                   parm      *blanks       esito
089900010205     C                   if        %error
090000010205     C                             or cmzerr = '1'
090100010205     C                             or esito  = '1'
090200010205     C                   eval      wrkesito = '3'
090300010205     C                   else
090400010201     C*
090500021113     C* 2° invio FIVAB
090600100525     C                   if        *in51 = *off
090700010201     C                   reset                   dscmz
090800110411     C                   move      vabfgs        cmzdst
090900010201     C                   eval      cmzfld = vlrfou
091000010201     C                   eval      cmzmbd = vlrhdl
091100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
091200021009     C***                if        prmfir = *blanks
091300021113     C                   eval      cmzfla = 'FIVAB00F'
091400021113     C                   eval      cmzmba = 'FIVAB00F'
091500021009     C***                else
091600021009     C***                eval      cmzfla = prmfir
091700021009     C***                eval      cmzmba = prmfir
091800021009     C***                endif
091900010201     C                   eval      cmznrr = *zeros
092000010201     C                   move      §ctrokvb      cmznrr
092100021018     C                   eval      cmzlba = vlrfl1
092200010201     C                   call(e)   'TIS711C'
092300010201     C                   parm                    dscmz
092400010201     C                   parm      *blanks       esito
092500010201     C                   if        %error
092600010201     C                             or cmzerr = '1'
092700010201     C                             or esito  = '1'
092800010201     C                   eval      wrkesito = '3'
092900010201     C                   endif
093000100525     C                   endif
093100010205     C                   endif
093200990920     C*
093300000613     C                   ENDSR
093400000613     C***
093500070411
093600110627     C     *pssr         BEGSR
093700070411     C*
093800110627     C                   if        %open(tivin00r)
093900110627     C                   close     tivin00r
094000110627     C                   endif
094100110627     C                   if        %open(fivabwwr)
094200110627     C                   close     fivabwwr
094300110627     C                   endif
094400110627     C                   if        %open(fivatwwr)
094500110627     C                   close     fivatwwr
094600110627     C                   endif
094700110627     C*
094800110627     C* Effettuo la chiamata al CLLE preposto
094900110627     C  N51              call(e)   'TITVVTC'
095000110627     C                   parm                    parccm
095100110627     C                   parm                    parmbr
095200110627     C                   parm      '2'           paropz
095300110627     C   51              call(e)   'TITVVBC'
095400110627     C                   parm                    parccm
095500110627     C                   parm                    parmbr
095600110627     C                   parm      '2'           paropz
095700110627     C*
095800110627     C                   eval      wrkesito = '2'
095900110627     C*
096000110627     C                   seton                                        LR
096100110627     C*
096200110627     C                   ENDSR     '*CANCL'
096300070411     C***
096400070411
096500990910
096600000613     C     *inzsr        BEGSR
096700990910     C*
096800990910     C     *entry        plist
096900990920     C                   parm                    tivlrds
097000990921     C                   parm      wrkesito      esito
097100000724     C                   parm                    prmlit
097200000710     C                   parm                    prmfir
097300000613     C*
097400000830     C* CALCOLA LA DATA CORRENTE
097500091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
097600091223     C                   eval      datcor = %dec(%date() : *ISO)
097700000830     C*
097800000613     C                   ENDSR
097900000613     C***
098000990908
098100110801     Otitv2h4p  E            riepilogo         2
098200990915     O                                              'Upload via Internet'
098300990915     O                                           +1 'Traduzione TIVIN00R -'
098400021113     O                                           +1 'FIVABWWR/FIVATWWR'
098500010201     O                                           +1 'Report testate bolle'
098600990915     O          E            riepilogo   2
098700990915     O                       wrkdata
098800990915     O                       wrkora              +1
098900990915     O                       procname            +1
099000990915     O          E            riepilogo   2
099100990915     O                                              'Cliente..................:'
099200990915     O                       VABCCM        z     +1
099300990915     O          E            riepilogo   2
099400990920     O                                              'Riferimento Strategi.....:'
099500990920     O                       vlrhdl              +1
099600990915     O          E            riepilogo   2
099700990915     O                                              'Giusti...................:'
099800010201     O                       §CTROKVB      2   +  1
099900100525     O                       §CTROKVT      2   +  1
100000990915     O          E            riepilogo   2
100100990915     O                                              'Sbagliati e corretti.....:'
100200971022     O                       §CTRMO        2   +  1
100300990915     O          E            riepilogo   2
100400990915     O                                              'Sbagliati e scartati.....:'
100500971022     O                       §CTRNO        2   +  1
100600000613
100700110801     Otitv2h4ps E            testdett          2
100800000613     O                                              'Upload via Internet'
100900000613     O                                           +1 'Traduzione TIVIN00R -'
101000021113     O                                           +1 'FIVABWWR/FIVATWWR'
101100010201     O                                           +1 'Report testate bolle'
101200000616     O          E            testdett    3
101300000613     O                                           +2 'N° rec'
101400000613     O                                           +3 'Anteprima contenuto'
101500000616     O          E            rigadett    2
101600000613     O                       rrnum               +2
101700000621     O                       recko               +3
101800000616     O          E            findett     2
101900000613     O                       wrkdata
102000000613     O                       wrkora              +1
102100000613     O                       procname            +1
102200000616     O          E            findett     2
102300000613     O                                              'Cliente..................:'
102400000613     O                       VABCCM        z     +1
102500000616     O          E            findett     2
102600000613     O                                              'Riferimento Strategi.....:'
102700000613     O                       vlrhdl              +1
102800000616     O          E            findett     2
102900000613     O                                              'Giusti...................:'
103000010201     O                       §CTROKVB      2   +  1
103100100525     O                       §CTROKVT      2   +  1
103200000616     O          E            findett     2
103300000613     O                                              'Sbagliati e corretti.....:'
103400000613     O                       §CTRMO        2   +  1
103500000616     O          E            findett     2
103600000613     O                                              'Sbagliati e scartati.....:'
103700000613     O                       §CTRNO        2   +  1
103800000512** CMD - COMANDI CL
103900110801OVRPRTF FILE(TITV2H4P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
104000110801OVRPRTF FILE(TITV2H4PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
104100110801DLTOVR FILE(TITV2H4P)  LVL(*)
104200110801DLTOVR FILE(TITV2H4PS) LVL(*)
104300000512
