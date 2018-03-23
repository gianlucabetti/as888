000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600100412     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800110405     Ftitv2d6p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100110405     Ftitv2d6ps O    f  198        PRINTER usropn
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
003600110405     D depspe          s             15    INZ(*blanks)
003700110405     D curspe          s             15    INZ(*blanks)
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
005100110405     D  VINSPE                       15
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
012400110405     C                   if        not %open(titv2d6ps)
012500110405     C                   open      titv2d6ps
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
014900110405     C                   if        %open(titv2d6ps)
015000000616     C                   except    findett
015100110405     C                   close     titv2d6ps
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
017000110405     C                   if        not %open(titv2d6p)
017100110405     C                   open      titv2d6p
017200990915     C                   endif
017300990915     C*
017400990915     C                   except    riepilogo
017500990915     C*
017600110405     C                   if        %open(titv2d6p)
017700110405     C                   close     titv2d6p
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
021600110405     C+ vindta, vinflg, substr(vindta, 35, 15) as sped, rrn(tivin00r)
021700060223     C+ from tivin00r
021800110405     C+ order by sped
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
024600110405     C                   EVAL      curspe=%trim(%subst(vindta:35:15))
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
030200110201     C*
030300071009     C                   endif
030400000905     C*
030500000905     C                   else
030600020725     C                   eval      x_vinflg = '1'
030700000905     C                   endif
030800000905     C*
030900020725     C     VINRRN        chain     tivin000
031000020725     C                   if        %found(tivin00r)
031100020725     C                   eval      y_vinflg = x_vinflg
031200020725     C                   eval      y_vinmsg = x_vinmsg
031300020725     C                   update    tivin000
031400020725     C                   endif
031500020725     C*
031600020725     C/EXEC SQL
031700070530     C+ Fetch C1 into :INPUT_DS
031800020725     C/END-EXEC
031900020725     C*
032000020725     C                   enddo
032100020725     C*
032200020725     C/EXEC SQL
032300020725     C+ close C1
032400020725     C/END-EXEC
032500000905     C*
032600020305     C* Scarico i VAB rimasti "in sospeso"
032700071009     C                   if        wDISK = 'C'
032800100412     C                   exsr      wrivab
032900071009     C                   endif
033000010202     C*
033100010202     C                   endif
033200990910
033300990910     C* Se non ci sono record con errori ...
033400000710     C                   if        §ctrno = 0
033500990910     C* ... restituisco esito OK.
033600990921     C                   eval      wrkesito = '0'
033700990910     C                   else
033800100525     C                   if        §ctrokvb > 0 OR
033900100525     C                             §ctrokvt > 0
034000990921     C                   eval      wrkesito = '1'
034100000710     C                   else
034200000710     C                   eval      wrkesito = '2'
034300990910     C                   endif
034400000710     C                   endif
034500990910     C*
034600990914     C                   if        %open(tivin00r)
034700990908     C                   close     tivin00r
034800990914     C                   endif
034900021113     C                   if        %open(fivabwwr)
035000021113     C                   close     fivabwwr
035100990914     C                   endif
035200021113     C                   if        %open(fivatwwr)
035300021113     C                   close     fivatwwr
035400010201     C                   endif
035500990910     C*
035600100525     C                   if        §ctrokvb > 0 OR
035700100525     C                             §ctrokvt > 0
035800000724     C                             and vlrpoi <> *zeros
035900010202     C                   exsr      invio
036000990920     C                   endif
036100990920     C*
036200910830     C                   ENDSR
036300000613     C***
036400100412
036500100412
036600010305
036700010305     C*----------------------------------------------------*
036800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
036900010305     C*----------------------------------------------------*
037000020305     C     WRIVAB        BEGSR
037100100412     C*
037200100412     C* Gestisco l'eventuale rottura x numero spedizione
037300100412     C                   if        VABRMA = *blanks
037400100412     C                   movel(P)  VABRMN        VABRMA
037500100412     C                   endif
037600100412     C*
037700100525     C                   if        *in51 = *off and *in31 = *off
037800100525     C                   write     fivab000                                     => scarico il VAB
037900100525     C                   endif
038000010305     C*
038100010305     C                   ENDSR
038200990920
038300000801     C*----------------------------------------------------*
038400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
038500000801     C*----------------------------------------------------*
038600010201     C     INZVAR        BEGSR
038700000801     C*
038800010201     C                   Z-ADD     *zeros        Num5_0
038900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
039000020725     C                   MOVEL     '0'           FlgCAS            1
039100000801     C*
039200000801     C                   ENDSR
039300000801     C*----------------------------------------------------*
039400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
039500000801     C*----------------------------------------------------*
039600020904     C     DEFCAM        BEGSR
039700080923     C*
039800100928     C                   SETOFF                                       505160
039900000801     C*
040000021113     C                   CLEAR                   FIVAB000
040100021113     C                   CLEAR                   FIVAT000
040200070730     C* Imposto i valori di default...
040300110405     C                   EVAL      VABCCM = 2771332
040400110405     C                   EVAL      VATCCM = 2771332
040500110405     C                   EVAL      VABLNP = 277
040600110405     C                   EVAL      VATLNP = 277
040700110128     C                   EVAL      VABCTR = 000
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
049900110405     C*
050000110405     C* Considerazioni sulla ragione sociale del destinatario da indicare
050100110405     C                   EVAL      VABRSD=%trim(%subst(vindta:50:35))
050200110405     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
050300110405     C     '@':'A'       XLATE     VABRSD        VABRSD
050400110405     C* ==
050500110405     C                   EVAL      VABIND=%trim(%subst(vindta:85:35))
050600110405     C                   EVAL      VABLOD=%trim(%subst(vindta:120:25))
050700110405     C                   EVAL      VABPRD=%trim(%subst(vindta:154:2))
050800110405     C                   EVAL      VABNZD=%trim(%subst(vindta:156:3))
050900110405     C                   EVAL      VABRMA=%trim(%subst(vindta:35:15))
051000110405     C                   EVAL      VABNOT=%trim(%subst(vindta:306:50))
051100110405     C                   EVAL      VABNT2=%trim(%subst(vindta:427:50))
051200110405     C                   IF        %subst(vindta:210:1) = 'M'
051300110405     C                   EVAL      VABTIC='BM'
051400110405     C                   ENDIF
051500110405     C                   IF        %subst(vindta:210:1) = 'N'
051600110405     C                   EVAL      VABTIC='TM'
051700110405     C                   ENDIF
051800110405     C                   IF        %subst(vindta:210:1) = 'R'
051900110405     C                   EVAL      VABTIC='CM'
052000110405     C                   ENDIF
052100110405     C                   IF        %subst(vindta:227:4) = *blanks
052200110405     C                   EVAL      VABVCA='EUR'
052300110405     C                   ENDIF
052400110405     C                   IF        %subst(vindta:231:1) = 'I'
052500110405     C                   EVAL      VABTC1='A'
052600110405     C                   ENDIF
052700110405     C                   IF        %subst(vindta:231:1) = '2'
052800110405     C                   EVAL      VABTC2='P'
052900110405     C                   ENDIF
053000110405     C                   IF        %subst(vindta:231:1) = '1'
053100110405     C                   EVAL      VABTC2='S'
053200110405     C                   ENDIF
053300100729     C*
053400100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
053500100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
053600100729     C     '@':'A'       XLATE     VABRSD        VABRSD
053700100729     C* ==
053800100729     C*
053900100729     C* Reperimento campi NUMERICI
054000100729     C                   MOVEL     DATCOR        VABAAS
054100100729     C                   MOVE      DATCOR        VABMGS
054200110405     C* DCR
054300110405     C                   IF        %subst(vindta:11:8) <> *blanks
054400110405     C                   EVAL      PiStr=%trim(%subst(vindta:11:8))
054500110405     C                   EXSR      CHKNUM
054600110405     C                   IF        PiInt=*on
054700110405     C                   Z-ADD     PiVal         VABDCR
054800110405     C                   ELSE
054900110405     C                   Z-ADD     *zeros        VABDCR
055000110406     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055100110405     C                             + ' ' + 'VABDCR'
055200110405     C                   ENDIF
055300110405     C                   ENDIF
055400110405     C* CAD
055500110405     C                   EVAL      PiStr=%trim(%subst(vindta:145:9))
055600110405     C                   EXSR      CHKNUM
055700110405     C                   IF        PiInt=*on
055800110405     C                   Z-ADD     PiVal         Num5_0
055900110405     C                   MOVEL(p)  Num5_0        VABCAD
056000110405     C                   ELSE
056100110405     C                   SETON                                        32
056200110405     C                   EVAL      VABCAD = *zeros
056300110406     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056400110405     C                             + ' ' + 'VABCAD'
056500110405     C                   ENDIF
056600110127     C* NSP / RMN
056700101119     C                   IF        *IN51 = *off
056800110405     C                   EVAL      PiStr=%trim(%subst(vindta:35:15))
056900100730     C                   EXSR      CHKNUM
057000100730     C                   IF        PiInt=*on
057100100730     C                   Z-ADD     PiVal         VABRMN
057200100928     C  N60              Z-ADD     PiVal         VABNSP
057300100928     C  N60              Z-ADD     PiVal         VATNSP
057400100730     C                   ELSE
057500100730     C                   SETON                                        32
057600100730     C                   Z-ADD     1             VABRMN
057700100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
057800101119     C                             + ' ' + 'VABNSP VABRMN VATNSP'
057900100730     C                   ENDIF
058000101119     C*
058100101119     C                   ELSE
058200101119     C*
058300110405     C                   EVAL      PiStr=%trim(%subst(vindta:35:15))
058400101119     C                   EXSR      CHKNUM
058500101119     C                   IF        PiInt=*on
058600101119     C  N60              Z-ADD     PiVal         VATNSP
058700101119     C                   ELSE
058800101119     C                   SETON                                        32
058900101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
059000101119     C                             + ' ' + 'VATNSP'
059100101119     C                   ENDIF
059200101119     C                   ENDIF
059300110201     C* NCL
059400110405     C                   EVAL      PiStr=%trim(%subst(vindta:164:5))
059500100928     C                   EXSR      CHKNUM
059600100928     C                   IF        PiInt=*on
059700110325     C                   Z-ADD     PiVal         VABNCL
059800100928     C                   ELSE
059900101119     C  N51              SETON                                        32
060000101119     C  N51              Z-ADD     *zeros        VABNCL
060100101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
060200100928     C                             + ' ' + 'VABNCL'
060300100928     C                   ENDIF
060400100729     C* PKB
060500110405     C                   IF        %subst(vindta:159:5) <> *all'0' AND
060600110405     C                             %subst(vindta:159:5) <> *blanks
060700110405     C                   EVAL      PiStr=%trim(%subst(vindta:159:5))
060800100729     C                   EXSR      CHKNUM
060900100729     C                   IF        PiNum=*on
061000110325     C                   Z-ADD(H)  PiVal         VABPKB
061100100729     C                   ELSE
061200101119     C  N51              SETON                                        32
061300101119     C  N51              Z-ADD     *zeros        VABPKB
061400101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
061500100729     C                             + ' ' + 'VABPKB'
061600100729     C                   ENDIF
061700110325     C                   ENDIF
061800110325     C* VLB
061900110405     C                   IF        %subst(vindta:184:6) <> *all'0' AND
062000110405     C                             %subst(vindta:184:6) <> *blanks
062100110405     C                   EVAL      PiStr=%trim(%subst(vindta:184:6))
062200110325     C                   EXSR      CHKNUM
062300110325     C                   IF        PiNum=*on
062400110325     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
062500110325     C                   Z-ADD(H)  PiVal         VABVLB
062600110325     C                   ELSE
062700110325     C  N51              SETON                                        32
062800110325     C  N51              Z-ADD     *zeros        VABVLB
062900110325     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
063000110325     C                             + ' ' + 'VABVLB'
063100110325     C                   ENDIF
063200110325     C                   ENDIF
063300100729     C* CAS
063400110405     C                   IF        %subst(vindta:196:14) <> *blanks AND
063500110405     C                             %subst(vindta:196:14) <> *all'0'
063600110405     C                   EVAL      FlgCAS = '1'
063700110405     C                   EVAL      PiStr=%trim(%subst(vindta:196:14))
063800100729     C                   EXSR      CHKNUM
063900100729     C                   IF        PiNum=*on
064000110325     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
064100100928     C                   Z-ADD(H)  PiVal         VABCAS
064200100729     C                   ELSE
064300101119     C  N51              SETON                                        32
064400101119     C  N51              Z-ADD     *zeros        VABCAS
064500101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
064600100729     C                             + ' ' + 'VABCAS'
064700100729     C                   ENDIF
064800101119     C                   ELSE
064900101119     C                   EVAL      FlgCAS = '0'
065000100729     C                   ENDIF
065100100729     C*
065200100729     C* Se provincia nn valorizzata la reperisco
065300100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
065400100729     C                   IF        VABNZD  = *blanks AND
065500100729     C                             VABPRD  = *blanks AND
065600100729     C                             VABCAD <> *blanks
065700100729     C                   CLEAR                   TISI95DS
065800100729     C                   EVAL      I95TCN = '3'
065900100729     C                   Z-ADD     datcor        I95DAT
066000100729     C                   EVAL      I95NAR = VABNZD
066100100729     C                   EVAL      I95CAP = VABCAD
066200100729     C                   EVAL      I95LOC = VABLOD
066300100729     C                   CALL      'TISI95R'
066400100729     C                   PARM                    TISI95DS
066500100729     C                   EVAL      VABPRD = O95PRV
066600100729     C                   ENDIF
066700100729     C*
066800100729     C* Considerazioni finali su CBO/CAS
066900100729     C                   IF        FlgCAS = '1'
067000100729     C                   IF        VABCBO = '1'
067100100729     C                   EVAL      VABCBO = '4'
067200100729     C                   ENDIF
067300100729     C                   IF        VABCBO = '2'
067400100729     C                   EVAL      VABCBO = '6'
067500100729     C                   ENDIF
067600100729     C                   ENDIF
067700010202     C*
067800000801     C* Ebbene...
067900000801     C                   ADD       1             §CTRMO
068000070530     C                   IF        *in31 <> *zeros OR
068100070530     C                             *in32 <> *zeros
068200000801     C                   ADD       1             §CTRNO
068300000801     C                   EVAL      recko = vindta
068400000801     C                   EXCEPT    rigadett
068500020725     C                   EVAL      x_vinflg = '2'
068600000801     C                   ELSE
068700010201     C                   ADD       1             §CTROKVB
068800000801     C                   ENDIF
068900000801     C*
069000000801     C                   ENDSR
069100100127     C*----------------------------------------------------*
069200100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
069300100127     C*----------------------------------------------------*
069400100127     C     WRIVAT_A      BEGSR
069500100127     C*
069600100127     C* Valorizzo l buffer di scrittura del FIVAT
069700100127     C* - TIPO RECORD "A"
069800101119     C***                eval      VATTRC = 'A'
069900101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
070000101119     C***                exsr      wrivat
070100100127     C*
070200100127     C                   ENDSR
070300010201     C*----------------------------------------------------*
070400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
070500010201     C*----------------------------------------------------*
070600071003     C     WRIVAT_B      BEGSR
070700010201     C*
070800021113     C* Valorizzo l buffer di scrittura del FIVAT
070900070928     C* - TIPO RECORD "B"
071000110325     C                   eval      VATTRC = 'B'
071100110405     C                   eval      VATNOT = %trim(%subst(vindta:407:20))
071200110325     C                   exsr      wrivat
071300010201     C*
071400010201     C                   ENDSR
071500071003     C*----------------------------------------------------*
071600071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
071700071003     C*----------------------------------------------------*
071800071003     C     WRIVAT_E      BEGSR
071900071003     C*
072000071003     C* Valorizzo l buffer di scrittura del FIVAT
072100071003     C* - TIPO RECORD "E"
072200100729     C*
072300100928     C                   eval      VATTRC = 'E'
072400110325     C                   eval      VATNOT = %trim(%subst(vindta:430:35))
072500100928     C                   exsr      wrivat
072600071003     C*
072700071003     C                   ENDSR
072800100127     C*----------------------------------------------------*
072900100127     C*  SCARICO BUFFER FIVAT
073000100127     C*----------------------------------------------------*
073100100127     C     WRIVAT        BEGSR
073200100127     C*
073300100127     C                   if        vatNOT <> *blanks
073400100127     C                   ADD       1             §CTROKVT
073500100127     C                   write     FIVAT000
073600100127     C                   endif
073700100127     C*
073800100127     C                   ENDSR
073900020904
074000020904
074100010202     C*----------------------------------------------------*
074200100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
074300010202     C*----------------------------------------------------*
074400100525     C     PREVASX       BEGSR
074500100525     C*
074600021113     C* Compongo il nome del membro da dare al FIVATWWR
074700010202     C                   eval      parmbr = vlrhdl
074800010202     C                   movel     'M'           parmbr
074900070530     C                   eval      parccm = %subst(vlrKSC:2:7)
075000010202     C                   eval      paropz = '1'
075100100525     C*
075200010202     C* Effettuo la chiamata al CLLE preposto
075300100525     C  N51              call(e)   'TITVVTC'
075400010202     C                   parm                    parccm
075500010202     C                   parm                    parmbr
075600010202     C                   parm                    paropz
075700100525     C   51              call(e)   'TITVVBC'
075800100525     C                   parm                    parccm
075900100525     C                   parm                    parmbr
076000100525     C                   parm                    paropz
076100100525     C*
076200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
076300010202     C                   if        %error
076400010202     C                   movel     '1'           chkcall
076500010202     C                   else
076600010202     C                   movel     '0'           chkcall
076700010202     C                   endif
076800010202     C*
076900010202     C                   ENDSR
077000000801     C*----------------------------------------------------*
077100000801     C*  CONTROLLO NUMERICITA' CAMPI
077200000801     C*----------------------------------------------------*
077300000801     C     CHKNUM        BEGSR
077400081113     C*
077500081113     C                   IF        PiDecChr = *blanks
077600110201     C                   EVAL      PiDecChr = ','
077700081113     C                   ENDIF
077800091223     C*
077900081113     C                   callp(e)  UBISNUM_Check(PiStr
078000081113     C                                          :PiDecChr
078100081113     C                                          :PiVal
078200081113     C                                          :PiNum
078300081113     C                                          :PiInt)
078400081113     C*
078500000801     C                   IF        %error
078600000801     C                   EVAL      PiInt=*off
078700000801     C                   ENDIF
078800000801     C*
078900000801     C                   ENDSR
079000000801     C***
079100011113
079200011113
079300000801
079400000801
079500990920      /TITLE Invio dei dati al punto operativo.
079600010202     C     invio         BEGSR
079700990920     C*
079800021113     C* 1° invio FIVAT
079900010201     C                   reset                   dscmz
080000010201     C                   move      vlrpoi        cmzdst
080100021113     C                   eval      cmzfld = 'FIVATWWR'
080200010201     C                   eval      cmzmbd = vlrhdl
080300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
080400021009     C***                if        prmfir = *blanks
080500021113     C                   eval      cmzfla = 'FIVAT00F'
080600021113     C                   eval      cmzmba = 'FIVAT00F'
080700021009     C***                else
080800021009     C***                eval      cmzfla = prmfir
080900021009     C***                eval      cmzmba = prmfir
081000021009     C***                endif
081100010201     C                   eval      cmznrr = *zeros
081200020305     C                   move      §ctrokvt      cmznrr
081300021018     C                   eval      cmzlba = vlrfl1
081400010201     C                   call(e)   'TIS711C'
081500010201     C                   parm                    dscmz
081600010201     C                   parm      *blanks       esito
081700010205     C                   if        %error
081800010205     C                             or cmzerr = '1'
081900010205     C                             or esito  = '1'
082000010205     C                   eval      wrkesito = '3'
082100010205     C                   else
082200010201     C*
082300021113     C* 2° invio FIVAB
082400100525     C                   if        *in51 = *off
082500010201     C                   reset                   dscmz
082600010201     C                   move      vlrpoi        cmzdst
082700010201     C                   eval      cmzfld = vlrfou
082800010201     C                   eval      cmzmbd = vlrhdl
082900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
083000021009     C***                if        prmfir = *blanks
083100021113     C                   eval      cmzfla = 'FIVAB00F'
083200021113     C                   eval      cmzmba = 'FIVAB00F'
083300021009     C***                else
083400021009     C***                eval      cmzfla = prmfir
083500021009     C***                eval      cmzmba = prmfir
083600021009     C***                endif
083700010201     C                   eval      cmznrr = *zeros
083800010201     C                   move      §ctrokvb      cmznrr
083900021018     C                   eval      cmzlba = vlrfl1
084000010201     C                   call(e)   'TIS711C'
084100010201     C                   parm                    dscmz
084200010201     C                   parm      *blanks       esito
084300010201     C                   if        %error
084400010201     C                             or cmzerr = '1'
084500010201     C                             or esito  = '1'
084600010201     C                   eval      wrkesito = '3'
084700010201     C                   endif
084800100525     C                   endif
084900010205     C                   endif
085000990920     C*
085100000613     C                   ENDSR
085200000613     C***
085300070411
085400070411     C     *pssr         BEGSR
085500070411     C*
085600070411     C                   if        %open(tivin00r)
085700070411     C                   close     tivin00r
085800070411     C                   endif
085900070411     C                   if        %open(fivabwwr)
086000070411     C                   close     fivabwwr
086100070411     C                   endif
086200070411     C                   if        %open(fivatwwr)
086300070411     C                   close     fivatwwr
086400070411     C                   endif
086500070411     C*
086600070411     C* Effettuo la chiamata al CLLE preposto
086700100525     C  N51              call(e)   'TITVVTC'
086800100525     C                   parm                    parccm
086900100525     C                   parm                    parmbr
087000100525     C                   parm      '2'           paropz
087100100525     C   51              call(e)   'TITVVBC'
087200100525     C                   parm                    parccm
087300100525     C                   parm                    parmbr
087400100525     C                   parm      '2'           paropz
087500070411     C*
087600070411     C                   eval      wrkesito = '2'
087700070411     C*
087800070411     C                   seton                                        LR
087900070411     C*
088000070411     C                   ENDSR     '*CANCL'
088100070411     C***
088200070411
088300990910
088400000613     C     *inzsr        BEGSR
088500990910     C*
088600990910     C     *entry        plist
088700990920     C                   parm                    tivlrds
088800990921     C                   parm      wrkesito      esito
088900000724     C                   parm                    prmlit
089000000710     C                   parm                    prmfir
089100000613     C*
089200000830     C* CALCOLA LA DATA CORRENTE
089300091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
089400091223     C                   eval      datcor = %dec(%date() : *ISO)
089500000830     C*
089600000613     C                   ENDSR
089700000613     C***
089800990908
089900110405     Otitv2d6p  E            riepilogo         2
090000990915     O                                              'Upload via Internet'
090100990915     O                                           +1 'Traduzione TIVIN00R -'
090200021113     O                                           +1 'FIVABWWR/FIVATWWR'
090300010201     O                                           +1 'Report testate bolle'
090400990915     O          E            riepilogo   2
090500990915     O                       wrkdata
090600990915     O                       wrkora              +1
090700990915     O                       procname            +1
090800990915     O          E            riepilogo   2
090900990915     O                                              'Cliente..................:'
091000990915     O                       VABCCM        z     +1
091100990915     O          E            riepilogo   2
091200990920     O                                              'Riferimento Strategi.....:'
091300990920     O                       vlrhdl              +1
091400990915     O          E            riepilogo   2
091500990915     O                                              'Giusti...................:'
091600010201     O                       §CTROKVB      2   +  1
091700100525     O                       §CTROKVT      2   +  1
091800990915     O          E            riepilogo   2
091900990915     O                                              'Sbagliati e corretti.....:'
092000971022     O                       §CTRMO        2   +  1
092100990915     O          E            riepilogo   2
092200990915     O                                              'Sbagliati e scartati.....:'
092300971022     O                       §CTRNO        2   +  1
092400000613
092500110405     Otitv2d6ps E            testdett          2
092600000613     O                                              'Upload via Internet'
092700000613     O                                           +1 'Traduzione TIVIN00R -'
092800021113     O                                           +1 'FIVABWWR/FIVATWWR'
092900010201     O                                           +1 'Report testate bolle'
093000000616     O          E            testdett    3
093100000613     O                                           +2 'N° rec'
093200000613     O                                           +3 'Anteprima contenuto'
093300000616     O          E            rigadett    2
093400000613     O                       rrnum               +2
093500000621     O                       recko               +3
093600000616     O          E            findett     2
093700000613     O                       wrkdata
093800000613     O                       wrkora              +1
093900000613     O                       procname            +1
094000000616     O          E            findett     2
094100000613     O                                              'Cliente..................:'
094200000613     O                       VABCCM        z     +1
094300000616     O          E            findett     2
094400000613     O                                              'Riferimento Strategi.....:'
094500000613     O                       vlrhdl              +1
094600000616     O          E            findett     2
094700000613     O                                              'Giusti...................:'
094800010201     O                       §CTROKVB      2   +  1
094900100525     O                       §CTROKVT      2   +  1
095000000616     O          E            findett     2
095100000613     O                                              'Sbagliati e corretti.....:'
095200000613     O                       §CTRMO        2   +  1
095300000616     O          E            findett     2
095400000613     O                                              'Sbagliati e scartati.....:'
095500000613     O                       §CTRNO        2   +  1
095600000512** CMD - COMANDI CL
095700110405OVRPRTF FILE(TITV2D6P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
095800110405OVRPRTF FILE(TITV2D6PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
095900110405DLTOVR FILE(TITV2D6P TITV2D6PS) LVL(*)
096000000512
096100000512
