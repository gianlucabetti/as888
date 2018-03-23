000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600100412     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800110127     Ftitv2c0p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100110127     Ftitv2c0ps O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
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
003600110127     D depspe          s              5    INZ(*blanks)
003700110127     D curspe          s              5    INZ(*blanks)
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
005100100729     D  VINSPE                        8
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
009600000724     C                   if        vlrpoi <> *zeros
009700000724     C*
009800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010000000724     C     vlrpoi        chain     azorg01l
010100000724     C                   if        %found
010200000616     C                   movel(p)  CMD(1)        depcmd
010300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010400000616     C*
010500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010600011113     C                   Z-ADD     150           LENGH            15 5
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
011900011113     C                   Z-ADD     150           LENGH            15 5
012000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012100000616     C                   PARM                    depcmd
012200000616     C                   PARM                    LENGH
012300000616     C*
012400110127     C                   if        not %open(titv2c0ps)
012500110127     C                   open      titv2c0ps
012600000616     C                   except    testdett
012700000613     C                   endif
012800000613     C*
012900100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
013000070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
013100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
013200000613     C*
013300010202     C* Effettuo la chiamata al CLLE preposto
013400100525     C  N51              call(e)   'TITVVTC'
013500100525     C                   parm                    parccm
013600100525     C                   parm                    parmbr
013700100525     C                   parm      '2'           paropz
013800100525     C   51              call(e)   'TITVVBC'
013900100525     C                   parm                    parccm
014000100525     C                   parm                    parmbr
014100100525     C                   parm      '2'           paropz
014200070730     C*
014300070730     C* Effettuo lancio TISI95 solo x chiusura
014400070730     C                   CLEAR                   TISI95DS
014500070730     C                   EVAL      I95TLA = 'C'
014600070730     C                   CALL      'TISI95R'
014700070730     C                   PARM                    TISI95DS
014800010202     C*
014900110127     C                   if        %open(titv2c0ps)
015000000616     C                   except    findett
015100110127     C                   close     titv2c0ps
015200000613     C                   endif
015300000616     C*
015400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015500011113     C                   Z-ADD     150           LENGH            15 5
015600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015700000616     C                   PARM                    CMD(3)
015800000616     C                   PARM                    LENGH
015900000616     C*
016000000801     C
016100010201     C                   seton                                        LR
016200000613
016300100310
016400100310
016500000613     C*--------------------------------------------------------
016600000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016700000613     C*--------------------------------------------------------
016800000621     C     STPR          BEGSR
016900000613     C*
017000110127     C                   if        not %open(titv2c0p)
017100110127     C                   open      titv2c0p
017200990915     C                   endif
017300990915     C*
017400990915     C                   except    riepilogo
017500990915     C*
017600110127     C                   if        %open(titv2c0p)
017700110127     C                   close     titv2c0p
017800990914     C                   endif
017900990910     C*
018000000613     C                   ENDSR
018100000613     C***
018200990908
018300000801
018400910830     C*--------------------------------------------------------
018500070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
018600910830     C*--------------------------------------------------------
018700070530     C     RWFILE        BEGSR
018800990910     C*
018900990914     C                   if        not %open(tivin00r)
019000990908     C                   open      tivin00r
019100990914     C                   endif
019200070530     C*
019300100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
019400100525     C                   exsr      prevasx
019500010201     C*
019600010202     C                   if        chkcall = '0'
019700010202     C*
019800100525     C                   if        not %open(fivabwwr)
019900100525     C                   open      fivabwwr
020000100525     C                   endif
020100100525     C*
020200021113     C                   if        not %open(fivatwwr)
020300021113     C                   open      fivatwwr
020400010201     C                   endif
020500080117     C*
020600080117     C                   EXSR      INZVAR
020700080117     C                   EXSR      DEFCAM
020800990910     C*
020900010201     C                   clear                   §CTROKVB          5 0
021000020305     C                   clear                   §CTROKVT          5 0
021100000801     C                   clear                   §CTRMO            5 0
021200000801     C                   clear                   §CTRNO            5 0
021300990910     C*
021400020725     C/EXEC SQL
021500020725     C+ declare C1 cursor for select
021600110127     C+ vindta, vinflg, substr(vindta, 320, 5) as sped, rrn(tivin00r)
021700060223     C+ from tivin00r
021800110127     C+ order by sped, substr(vindta, 320, 5) desc
021900020725     C+ for read only
022000020725     C/END-EXEC
022100020725     C
022200020725     C/EXEC SQL
022300020725     C+ open C1
022400020725     C/END-EXEC
022500020725     C
022600020725     C/EXEC SQL
022700070530     C+ Fetch C1 into :INPUT_DS
022800020725     C/END-EXEC
022900020725     C*
023000020725     C                   dow       sqlcod = *zeros
023100990913     C*
023200100310     C                   if        vindta > *blanks
023300000613     C                   add       1             rrnum
023400000801     C*
023500020725     C                   if        vinflg = *blanks
023600020725     C                             or vinflg = '0'
023700020725     C                             or vinflg = '2'
023800000801     C*
023900020725     C                   clear                   x_vinmsg
024000020725     C                   eval      x_vinflg = '1'
024100101119     C*
024200101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
024300101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
024400010305     C*
024500010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
024600110127     C                   EVAL      curspe=%trim(%subst(vindta:320:5))
024700010305     C*
024800100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
024900080923     C                   if        *in50 = *off
025000101119     C                   exsr      inzvar
025100101119     C                   exsr      defcam
025200071003     C                   exsr      impvab
025300100525     C                   exsr      wrivab
025400100127     C                   exsr      wrivat_a                                     => carico VAT
025500071003     C                   exsr      wrivat_b                                     => carico VAT
025600100525     C   51              exsr      wrivat_e                                     => carico VAT
025700071003     C                   else
025800080923     C*
025900071009     C                   if        wDISK = 'D'
026000091223     C                   exsr      repNSP
026100071009     C                   exsr      impvab
026200100412     C                   exsr      wrivab
026300100127     C                   exsr      wrivat_a                                     => carico VAT
026400071009     C                   exsr      wrivat_b                                     => carico VAT
026500071009     C                   exsr      wrivat_e                                     => carico VAT
026600071009     C                   else
026700071009     C*
026800010305     C                   if        depspe = *blanks                             => 1° giro
026900010305     C                   eval      depspe = curspe                              => memorizz. spediz
027000080117     C                   clear                   tivinds
027100091223     C                   exsr      repNSP
027200020305     C                   exsr      impvab
027300100127     C                   exsr      wrivat_a                                     => carico VAT
027400071003     C                   exsr      wrivat_b                                     => carico VAT
027500071003     C   50              exsr      wrivat_e                                     => carico VAT
027600010305     C                   else
027700020725     C                   if        depspe <> curspe                             => rottura di spediz
027800010305     C                   eval      depspe = curspe                              => memorizz. spediz
027900100412     C                   exsr      wrivab
028000080117     C                   clear                   tivinds
028100091223     C                   exsr      repNSP
028200020305     C                   exsr      impvab
028300100127     C                   exsr      wrivat_a                                     => carico VAT
028400071003     C                   exsr      wrivat_b                                     => carico VAT
028500071003     C   50              exsr      wrivat_e                                     => carico VAT
028600020305     C                   else                                                   => x stessa spediz
028700100401     C*
028800100401     C* Se nn richiesta esclusione spedizioni "duplicate"
028900100928     C                   select
029000100928     C                   when      wDUPREC = ' '
029100020305     C                   exsr      impvab
029200100127     C                   exsr      wrivat_a                                     => carico VAT
029300071003     C                   exsr      wrivat_b                                     => carico VAT
029400071003     C   50              exsr      wrivat_e                                     => carico VAT
029500100928     C                   when      wDUPREC = 'C'
029600100928     C   50              exsr      wrivat_e                                     => carico VAT
029700100928     C                   endsl
029800010305     C                   endif
029900010305     C                   endif
030000010305     C                   endif
030100071003     C                   endif
030200071009     C                   endif
030300000905     C*
030400000905     C                   else
030500020725     C                   eval      x_vinflg = '1'
030600000905     C                   endif
030700000905     C*
030800020725     C     VINRRN        chain     tivin000
030900020725     C                   if        %found(tivin00r)
031000020725     C                   eval      y_vinflg = x_vinflg
031100020725     C                   eval      y_vinmsg = x_vinmsg
031200020725     C                   update    tivin000
031300020725     C                   endif
031400020725     C*
031500020725     C/EXEC SQL
031600070530     C+ Fetch C1 into :INPUT_DS
031700020725     C/END-EXEC
031800020725     C*
031900020725     C                   enddo
032000020725     C*
032100020725     C/EXEC SQL
032200020725     C+ close C1
032300020725     C/END-EXEC
032400000905     C*
032500020305     C* Scarico i VAB rimasti "in sospeso"
032600071009     C                   if        wDISK = 'C'
032700100412     C                   exsr      wrivab
032800071009     C                   endif
032900010202     C*
033000010202     C                   endif
033100990910
033200990910     C* Se non ci sono record con errori ...
033300000710     C                   if        §ctrno = 0
033400990910     C* ... restituisco esito OK.
033500990921     C                   eval      wrkesito = '0'
033600990910     C                   else
033700100525     C                   if        §ctrokvb > 0 OR
033800100525     C                             §ctrokvt > 0
033900990921     C                   eval      wrkesito = '1'
034000000710     C                   else
034100000710     C                   eval      wrkesito = '2'
034200990910     C                   endif
034300000710     C                   endif
034400990910     C*
034500990914     C                   if        %open(tivin00r)
034600990908     C                   close     tivin00r
034700990914     C                   endif
034800021113     C                   if        %open(fivabwwr)
034900021113     C                   close     fivabwwr
035000990914     C                   endif
035100021113     C                   if        %open(fivatwwr)
035200021113     C                   close     fivatwwr
035300010201     C                   endif
035400990910     C*
035500100525     C                   if        §ctrokvb > 0 OR
035600100525     C                             §ctrokvt > 0
035700000724     C                             and vlrpoi <> *zeros
035800010202     C                   exsr      invio
035900990920     C                   endif
036000990920     C*
036100910830     C                   ENDSR
036200000613     C***
036300100412
036400100412
036500010305
036600010305     C*----------------------------------------------------*
036700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
036800010305     C*----------------------------------------------------*
036900020305     C     WRIVAB        BEGSR
037000100412     C*
037100100412     C* Gestisco l'eventuale rottura x numero spedizione
037200100412     C                   if        VABRMA = *blanks
037300100412     C                   movel(P)  VABRMN        VABRMA
037400100412     C                   endif
037500100412     C*
037600100525     C                   if        *in51 = *off and *in31 = *off
037700100525     C                   write     fivab000                                     => scarico il VAB
037800100525     C                   endif
037900010305     C*
038000010305     C                   ENDSR
038100990920
038200000801     C*----------------------------------------------------*
038300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
038400000801     C*----------------------------------------------------*
038500010201     C     INZVAR        BEGSR
038600000801     C*
038700010201     C                   Z-ADD     *zeros        Num5_0
038800020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
038900020725     C                   MOVEL     '0'           FlgCAS            1
039000000801     C*
039100000801     C                   ENDSR
039200000801     C*----------------------------------------------------*
039300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
039400000801     C*----------------------------------------------------*
039500020904     C     DEFCAM        BEGSR
039600080923     C*
039700100928     C                   SETOFF                                       505160
039800000801     C*
039900021113     C                   CLEAR                   FIVAB000
040000021113     C                   CLEAR                   FIVAT000
040100070730     C* Imposto i valori di default...
040200110128     C                   EVAL      VABCCM = 1161163
040300110128     C                   EVAL      VATCCM = 1161163
040400110128     C                   EVAL      VABLNP = 116
040500110128     C                   EVAL      VATLNP = 116
040600110128     C                   EVAL      VABCTR = 000
040700100928     C                   EVAL      VABCTM = '7Q'
040800100525     C                   EVAL      VABCBO = '1'
040900070222     C* ... e poi verifico se sono stati passati come parametri
041000070222     C                   IF        vlrppt > *blanks
041100100525     C*
041200070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
041300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
041400070222     C                   EXSR      CHKNUM
041500070222     C                   IF        PiInt=*on
041600070222     C                   Z-ADD     PiVal         VABCCM
041700070222     C                   Z-ADD     PiVal         VATCCM
041800070222     C                   ENDIF
041900070222     C                   ENDIF
042000070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
042100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
042200070222     C                   EXSR      CHKNUM
042300070222     C                   IF        PiInt=*on
042400070222     C                   Z-ADD     PiVal         VABLNP
042500070222     C                   Z-ADD     PiVal         VATLNP
042600070222     C                   ENDIF
042700070222     C                   ENDIF
042800070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
042900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
043000070222     C                   EXSR      CHKNUM
043100070222     C                   IF        PiInt=*on
043200070222     C                   Z-ADD     PiVal         VABCTR
043300070222     C                   ENDIF
043400070928     C                   ENDIF
043500071009     C                   MOVEL     *blanks       wDISK             1
043600071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
043700071009     C                   IF        wDISK <> *blanks
043800070928     C                   SETON                                        50
043900070222     C                   ENDIF
044000100525     C                   IF        wDISK  = 'T'
044100100525     C                   SETOFF                                       50
044200100525     C                   SETON                                        51
044300100525     C                   ENDIF
044400100401     C                   MOVEL     *blanks       wDUPREC           1
044500100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
044600100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
044700100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
044800100525     C                   ENDIF
044900100525     C*
045000070222     C                   ENDIF
045100071009     C*
045200071009     C   50              EVAL      VABCTM = '7Q'
045300000801     C*
045400000801     C                   ENDSR
045500091223     C*----------------------------------------------------*
045600091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
045700091223     C*----------------------------------------------------*
045800091223     C     REPNSP        BEGSR
045900091223     C*
046000091223     C                   EXSR      INZVAR
046100091223     C                   EXSR      DEFCAM
046200100928     C*
046300100928     C                   SETON                                        60
046400091223     C*
046500091223     C* NSP => Stacco un numeratore da AZNUM
046600091223     C                   clear                   TRUL33DS
046700091223     C                   eval      I33OPE = *zeros
046800091223     C                   eval      I33CNU = 302
046900091223     C                   eval      I33NUM = 1
047000091223     C                   movel     TRUL33DS      KPJBU
047100091223     C                   call      'TRUL33R'
047200091223     C                   parm                    KPJBA
047300091223     C                   movel     KPJBU         TRUL33DS
047400091223     C                   if        O33ERR = *zeros
047500091223     C                   z-add     O33NRF        VABNSP
047600091223     C                   z-add     O33NRF        VATNSP
047700091223     C                   else
047800091223     C                   SETON                                        31
047900091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048000091223     C                             + ' ' + 'VABNSP VATNSP'
048100091223     C                   endif
048200091223     C*
048300091223     C                   ENDSR
048400000801     C*----------------------------------------------------*
048500021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
048600000801     C*----------------------------------------------------*
048700100730     C     IMPVAB        BEGSR
048800070530     C*
048900070530     C                   SETOFF                                       3132
049000070928     C*
049100070928     C                   MOVE(P)   vlrpoi        VABFGS
049200070928     C                   MOVE(P)   vlrpoi        VATFGS
049300070928     C*
049400070928     C                   MOVEL     datcor        VABAAS
049500070928     C                   MOVEL     datcor        VATAAS
049600070928     C                   MOVE      datcor        VABMGS
049700100729     C*
049800100729     C* Reperimento campi ALFA
049900110127     C                   EVAL      VABRSD=%trim(%subst(vindta:33:40))
050000110127     C                   EVAL      VABIND=%trim(%subst(vindta:73:50))
050100110127     C                   EVAL      VABLOD=%trim(%subst(vindta:123:40))
050200110127     C                   EVAL      VABPRD=%trim(%subst(vindta:168:2))
050300110127     C                   EVAL      VABCAD=%trim(%subst(vindta:163:5))
050400101117     C                   EVAL      VABNZD=%trim(%subst(vindta:674:3))
050500101117     C                   IF        VABNZD='I'   or
050600101117     C                             VABNZD='IT'  or
050700101117     C                             VABNZD='ITA'
050800101117     C                   EVAL      VABNZD=*blanks
050900101117     C                   ENDIF
051000110128     C                   EVAL      VABCBO=%trim(%subst(vindta:68:2))
051100110127     C*
051200110127     C                   SELECT
051300110127     C                   WHEN      %subst(vindta:199:1) = 'C'
051400110127     C                   EVAL      VABTIC = 'CM'
051500110127     C                   WHEN      %subst(vindta:199:1) = 'D'
051600110127     C                   EVAL      VABTIC = 'TM'
051700110127     C                   WHEN      %subst(vindta:199:1) = 'V'
051800110127     C                   EVAL      VABTIC = 'BM'
051900110127     C                   ENDSL
052000110127     C*
052100110128     C                   EVAL      VABRMA=%trim(%subst(vindta:28:15))
052200110127     C                   EVAL      VABNOT=%trim(%subst(vindta:201:13))
052300100729     C*
052400100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
052500100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
052600100729     C     '@':'A'       XLATE     VABRSD        VABRSD
052700100729     C* ==
052800100729     C*
052900100729     C* Reperimento campi NUMERICI
053000100729     C                   MOVEL     DATCOR        VABAAS
053100100729     C                   MOVE      DATCOR        VABMGS
053200110127     C* NSP / RMN
053300101119     C                   IF        *IN51 = *off
053400110128     C                   EVAL      PiStr=%trim(%subst(vindta:13:15))
053500100730     C                   EXSR      CHKNUM
053600100730     C                   IF        PiInt=*on
053700100730     C                   Z-ADD     PiVal         VABRMN
053800100928     C  N60              Z-ADD     PiVal         VABNSP
053900100928     C  N60              Z-ADD     PiVal         VATNSP
054000100730     C                   ELSE
054100100730     C                   SETON                                        32
054200100730     C                   Z-ADD     1             VABRMN
054300100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054400101119     C                             + ' ' + 'VABNSP VABRMN VATNSP'
054500100730     C                   ENDIF
054600101119     C*
054700101119     C                   ELSE
054800101119     C*
054900110127     C                   EVAL      PiStr=%trim(%subst(vindta:320:5))
055000101119     C                   EXSR      CHKNUM
055100101119     C                   IF        PiInt=*on
055200101119     C  N60              Z-ADD     PiVal         VATNSP
055300101119     C                   ELSE
055400101119     C                   SETON                                        32
055500101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055600101119     C                             + ' ' + 'VATNSP'
055700101119     C                   ENDIF
055800101119     C                   ENDIF
055900100729     C* NCL
056000110127     C                   EVAL      PiStr=%trim(%subst(vindta:171:3))
056100100928     C                   EXSR      CHKNUM
056200100928     C                   IF        PiInt=*on
056300100928     C                   Z-ADD     PiVal         VABNCL
056400100928     C                   ELSE
056500101119     C  N51              SETON                                        32
056600101119     C  N51              Z-ADD     *zeros        VABNCL
056700101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
056800100928     C                             + ' ' + 'VABNCL'
056900100928     C                   ENDIF
057000100729     C* PKB
057100110127     C                   EVAL      PiStr=%trim(%subst(vindta:174:4))
057200100729     C                   EXSR      CHKNUM
057300100729     C                   IF        PiNum=*on
057400101119     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
057500100729     C                   Z-ADD(H)  PiVal         VABPKB
057600100729     C                   ELSE
057700101119     C  N51              SETON                                        32
057800101119     C  N51              Z-ADD     *zeros        VABPKB
057900101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
058000100729     C                             + ' ' + 'VABPKB'
058100100729     C                   ENDIF
058200101117     C* VLB
058300110127     C                   IF        %subst(vindta:178:5) <> '00.00'
058400110127     C                   EVAL      PiStr=%trim(%subst(vindta:178:5))
058500101117     C                   EXSR      CHKNUM
058600101117     C                   IF        PiNum=*on
058700101117     C                   Z-ADD(H)  PiVal         VABVLB
058800101117     C                   ELSE
058900101119     C  N51              SETON                                        32
059000101119     C  N51              Z-ADD     *zeros        VABVLB
059100101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
059200101117     C                             + ' ' + 'VABVLB'
059300101117     C                   ENDIF
059400101119     C                   ENDIF
059500100729     C* CAS
059600110127     C                   IF        %subst(vindta:183:8)<> *blanks    AND
059700110127     C                             %subst(vindta:183:8)<> *zeros     AND
059800110127     C                             %subst(vindta:183:8)<> '00000.00'
059900100729     C                   EVAL      FlgCAS = '1'
060000110127     C                   EVAL      VABVCA = 'EUR'
060100110127     C                   EVAL      PiStr=%trim(%subst(vindta:183:8))
060200100729     C                   EXSR      CHKNUM
060300100729     C                   IF        PiNum=*on
060400100928     C                   Z-ADD(H)  PiVal         VABCAS
060500100729     C                   ELSE
060600101119     C  N51              SETON                                        32
060700101119     C  N51              Z-ADD     *zeros        VABCAS
060800101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
060900100729     C                             + ' ' + 'VABCAS'
061000100729     C                   ENDIF
061100101119     C                   ELSE
061200101119     C                   EVAL      FlgCAS = '0'
061300100729     C                   ENDIF
061400100729     C*
061500100729     C* Se provincia nn valorizzata la reperisco
061600100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
061700100729     C                   IF        VABNZD  = *blanks AND
061800100729     C                             VABPRD  = *blanks AND
061900100729     C                             VABCAD <> *blanks
062000100729     C                   CLEAR                   TISI95DS
062100100729     C                   EVAL      I95TCN = '3'
062200100729     C                   Z-ADD     datcor        I95DAT
062300100729     C                   EVAL      I95NAR = VABNZD
062400100729     C                   EVAL      I95CAP = VABCAD
062500100729     C                   EVAL      I95LOC = VABLOD
062600100729     C                   CALL      'TISI95R'
062700100729     C                   PARM                    TISI95DS
062800100729     C                   EVAL      VABPRD = O95PRV
062900100729     C                   ENDIF
063000100729     C*
063100100729     C* Considerazioni finali su CBO/CAS
063200100729     C                   IF        FlgCAS = '1'
063300100729     C                   IF        VABCBO = '1'
063400100729     C                   EVAL      VABCBO = '4'
063500100729     C                   ENDIF
063600100729     C                   IF        VABCBO = '2'
063700100729     C                   EVAL      VABCBO = '6'
063800100729     C                   ENDIF
063900100729     C                   ENDIF
064000010202     C*
064100000801     C* Ebbene...
064200000801     C                   ADD       1             §CTRMO
064300070530     C                   IF        *in31 <> *zeros OR
064400070530     C                             *in32 <> *zeros
064500000801     C                   ADD       1             §CTRNO
064600000801     C                   EVAL      recko = vindta
064700000801     C                   EXCEPT    rigadett
064800020725     C                   EVAL      x_vinflg = '2'
064900000801     C                   ELSE
065000010201     C                   ADD       1             §CTROKVB
065100000801     C                   ENDIF
065200000801     C*
065300000801     C                   ENDSR
065400100127     C*----------------------------------------------------*
065500100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
065600100127     C*----------------------------------------------------*
065700100127     C     WRIVAT_A      BEGSR
065800100127     C*
065900100127     C* Valorizzo l buffer di scrittura del FIVAT
066000100127     C* - TIPO RECORD "A"
066100101119     C***                eval      VATTRC = 'A'
066200101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
066300101119     C***                exsr      wrivat
066400100127     C*
066500100127     C                   ENDSR
066600010201     C*----------------------------------------------------*
066700071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
066800010201     C*----------------------------------------------------*
066900071003     C     WRIVAT_B      BEGSR
067000010201     C*
067100021113     C* Valorizzo l buffer di scrittura del FIVAT
067200070928     C* - TIPO RECORD "B"
067300101119     C***                eval      VATTRC = 'B'
067400101119     C***                eval      VATNOT = %trim(%subst(vindta:726:12))
067500101119     C***                exsr      wrivat
067600010201     C*
067700010201     C                   ENDSR
067800071003     C*----------------------------------------------------*
067900071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
068000071003     C*----------------------------------------------------*
068100071003     C     WRIVAT_E      BEGSR
068200071003     C*
068300071003     C* Valorizzo l buffer di scrittura del FIVAT
068400071003     C* - TIPO RECORD "E"
068500100729     C*
068600100928     C                   eval      VATTRC = 'E'
068700110127     C                   eval      VATNOT = %trim(%subst(vindta:459:8))
068800100928     C                   exsr      wrivat
068900071003     C*
069000071003     C                   ENDSR
069100100127     C*----------------------------------------------------*
069200100127     C*  SCARICO BUFFER FIVAT
069300100127     C*----------------------------------------------------*
069400100127     C     WRIVAT        BEGSR
069500100127     C*
069600100127     C                   if        vatNOT <> *blanks
069700100127     C                   ADD       1             §CTROKVT
069800100127     C                   write     FIVAT000
069900100127     C                   endif
070000100127     C*
070100100127     C                   ENDSR
070200020904
070300020904
070400010202     C*----------------------------------------------------*
070500100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
070600010202     C*----------------------------------------------------*
070700100525     C     PREVASX       BEGSR
070800100525     C*
070900021113     C* Compongo il nome del membro da dare al FIVATWWR
071000010202     C                   eval      parmbr = vlrhdl
071100010202     C                   movel     'M'           parmbr
071200070530     C                   eval      parccm = %subst(vlrKSC:2:7)
071300010202     C                   eval      paropz = '1'
071400100525     C*
071500010202     C* Effettuo la chiamata al CLLE preposto
071600100525     C  N51              call(e)   'TITVVTC'
071700010202     C                   parm                    parccm
071800010202     C                   parm                    parmbr
071900010202     C                   parm                    paropz
072000100525     C   51              call(e)   'TITVVBC'
072100100525     C                   parm                    parccm
072200100525     C                   parm                    parmbr
072300100525     C                   parm                    paropz
072400100525     C*
072500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
072600010202     C                   if        %error
072700010202     C                   movel     '1'           chkcall
072800010202     C                   else
072900010202     C                   movel     '0'           chkcall
073000010202     C                   endif
073100010202     C*
073200010202     C                   ENDSR
073300000801     C*----------------------------------------------------*
073400000801     C*  CONTROLLO NUMERICITA' CAMPI
073500000801     C*----------------------------------------------------*
073600000801     C     CHKNUM        BEGSR
073700081113     C*
073800081113     C                   IF        PiDecChr = *blanks
073900100503     C                   EVAL      PiDecChr = '.'
074000081113     C                   ENDIF
074100091223     C*
074200081113     C                   callp(e)  UBISNUM_Check(PiStr
074300081113     C                                          :PiDecChr
074400081113     C                                          :PiVal
074500081113     C                                          :PiNum
074600081113     C                                          :PiInt)
074700081113     C*
074800000801     C                   IF        %error
074900000801     C                   EVAL      PiInt=*off
075000000801     C                   ENDIF
075100000801     C*
075200000801     C                   ENDSR
075300000801     C***
075400011113
075500011113
075600000801
075700000801
075800990920      /TITLE Invio dei dati al punto operativo.
075900010202     C     invio         BEGSR
076000990920     C*
076100021113     C* 1° invio FIVAT
076200010201     C                   reset                   dscmz
076300010201     C                   move      vlrpoi        cmzdst
076400021113     C                   eval      cmzfld = 'FIVATWWR'
076500010201     C                   eval      cmzmbd = vlrhdl
076600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
076700021009     C***                if        prmfir = *blanks
076800021113     C                   eval      cmzfla = 'FIVAT00F'
076900021113     C                   eval      cmzmba = 'FIVAT00F'
077000021009     C***                else
077100021009     C***                eval      cmzfla = prmfir
077200021009     C***                eval      cmzmba = prmfir
077300021009     C***                endif
077400010201     C                   eval      cmznrr = *zeros
077500020305     C                   move      §ctrokvt      cmznrr
077600021018     C                   eval      cmzlba = vlrfl1
077700010201     C                   call(e)   'TIS711C'
077800010201     C                   parm                    dscmz
077900010201     C                   parm      *blanks       esito
078000010205     C                   if        %error
078100010205     C                             or cmzerr = '1'
078200010205     C                             or esito  = '1'
078300010205     C                   eval      wrkesito = '3'
078400010205     C                   else
078500010201     C*
078600021113     C* 2° invio FIVAB
078700100525     C                   if        *in51 = *off
078800010201     C                   reset                   dscmz
078900010201     C                   move      vlrpoi        cmzdst
079000010201     C                   eval      cmzfld = vlrfou
079100010201     C                   eval      cmzmbd = vlrhdl
079200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
079300021009     C***                if        prmfir = *blanks
079400021113     C                   eval      cmzfla = 'FIVAB00F'
079500021113     C                   eval      cmzmba = 'FIVAB00F'
079600021009     C***                else
079700021009     C***                eval      cmzfla = prmfir
079800021009     C***                eval      cmzmba = prmfir
079900021009     C***                endif
080000010201     C                   eval      cmznrr = *zeros
080100010201     C                   move      §ctrokvb      cmznrr
080200021018     C                   eval      cmzlba = vlrfl1
080300010201     C                   call(e)   'TIS711C'
080400010201     C                   parm                    dscmz
080500010201     C                   parm      *blanks       esito
080600010201     C                   if        %error
080700010201     C                             or cmzerr = '1'
080800010201     C                             or esito  = '1'
080900010201     C                   eval      wrkesito = '3'
081000010201     C                   endif
081100100525     C                   endif
081200010205     C                   endif
081300990920     C*
081400000613     C                   ENDSR
081500000613     C***
081600070411
081700070411     C     *pssr         BEGSR
081800070411     C*
081900070411     C                   if        %open(tivin00r)
082000070411     C                   close     tivin00r
082100070411     C                   endif
082200070411     C                   if        %open(fivabwwr)
082300070411     C                   close     fivabwwr
082400070411     C                   endif
082500070411     C                   if        %open(fivatwwr)
082600070411     C                   close     fivatwwr
082700070411     C                   endif
082800070411     C*
082900070411     C* Effettuo la chiamata al CLLE preposto
083000100525     C  N51              call(e)   'TITVVTC'
083100100525     C                   parm                    parccm
083200100525     C                   parm                    parmbr
083300100525     C                   parm      '2'           paropz
083400100525     C   51              call(e)   'TITVVBC'
083500100525     C                   parm                    parccm
083600100525     C                   parm                    parmbr
083700100525     C                   parm      '2'           paropz
083800070411     C*
083900070411     C                   eval      wrkesito = '2'
084000070411     C*
084100070411     C                   seton                                        LR
084200070411     C*
084300070411     C                   ENDSR     '*CANCL'
084400070411     C***
084500070411
084600990910
084700000613     C     *inzsr        BEGSR
084800990910     C*
084900990910     C     *entry        plist
085000990920     C                   parm                    tivlrds
085100990921     C                   parm      wrkesito      esito
085200000724     C                   parm                    prmlit
085300000710     C                   parm                    prmfir
085400000613     C*
085500000830     C* CALCOLA LA DATA CORRENTE
085600091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
085700091223     C                   eval      datcor = %dec(%date() : *ISO)
085800000830     C*
085900000613     C                   ENDSR
086000000613     C***
086100990908
086200110127     Otitv2c0p  E            riepilogo         2
086300990915     O                                              'Upload via Internet'
086400990915     O                                           +1 'Traduzione TIVIN00R -'
086500021113     O                                           +1 'FIVABWWR/FIVATWWR'
086600010201     O                                           +1 'Report testate bolle'
086700990915     O          E            riepilogo   2
086800990915     O                       wrkdata
086900990915     O                       wrkora              +1
087000990915     O                       procname            +1
087100990915     O          E            riepilogo   2
087200990915     O                                              'Cliente..................:'
087300990915     O                       VABCCM        z     +1
087400990915     O          E            riepilogo   2
087500990920     O                                              'Riferimento Strategi.....:'
087600990920     O                       vlrhdl              +1
087700990915     O          E            riepilogo   2
087800990915     O                                              'Giusti...................:'
087900010201     O                       §CTROKVB      2   +  1
088000100525     O                       §CTROKVT      2   +  1
088100990915     O          E            riepilogo   2
088200990915     O                                              'Sbagliati e corretti.....:'
088300971022     O                       §CTRMO        2   +  1
088400990915     O          E            riepilogo   2
088500990915     O                                              'Sbagliati e scartati.....:'
088600971022     O                       §CTRNO        2   +  1
088700000613
088800110127     Otitv2c0ps E            testdett          2
088900000613     O                                              'Upload via Internet'
089000000613     O                                           +1 'Traduzione TIVIN00R -'
089100021113     O                                           +1 'FIVABWWR/FIVATWWR'
089200010201     O                                           +1 'Report testate bolle'
089300000616     O          E            testdett    3
089400000613     O                                           +2 'N° rec'
089500000613     O                                           +3 'Anteprima contenuto'
089600000616     O          E            rigadett    2
089700000613     O                       rrnum               +2
089800000621     O                       recko               +3
089900000616     O          E            findett     2
090000000613     O                       wrkdata
090100000613     O                       wrkora              +1
090200000613     O                       procname            +1
090300000616     O          E            findett     2
090400000613     O                                              'Cliente..................:'
090500000613     O                       VABCCM        z     +1
090600000616     O          E            findett     2
090700000613     O                                              'Riferimento Strategi.....:'
090800000613     O                       vlrhdl              +1
090900000616     O          E            findett     2
091000000613     O                                              'Giusti...................:'
091100010201     O                       §CTROKVB      2   +  1
091200100525     O                       §CTROKVT      2   +  1
091300000616     O          E            findett     2
091400000613     O                                              'Sbagliati e corretti.....:'
091500000613     O                       §CTRMO        2   +  1
091600000616     O          E            findett     2
091700000613     O                                              'Sbagliati e scartati.....:'
091800000613     O                       §CTRNO        2   +  1
091900000512** CMD - COMANDI CL
092000110127OVRPRTF FILE(TITV2C0P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
092100110127OVRPRTF FILE(TITV2C0PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
092200110127DLTOVR FILE(TITV2C0P TITV2C0PS) LVL(*)
092300000512
092400000512
