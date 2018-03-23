000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600100412     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800101116     Ftitv2b2p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100101116     Ftitv2b2ps O    f  198        PRINTER usropn
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
003600101117     D depspe          s             20    INZ(*blanks)
003700101117     D curspe          s             20    INZ(*blanks)
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
006400081113
006500081113     D*------------------
006600081113     D* LINKING A DEFINIZIONI ESTERNE
006700081113     D*------------------
006800081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006900090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007000081113
007100990908
007200010201
007300010201
007400080117     C*
007500080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007600080117     C
007700080117     C/EXEC SQL
007800080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007900080117     C/END-EXEC
008000080117     C
008100990915     C                   time                    wrkdata
008200990915     C                   time                    wrkora
008300000913     C                   reset                   rrnum
008400990921     C                   reset                   esito
008500990921     C                   reset                   wrkesito
008600000724     C*
008700000724     C* SE OCCORRE SPEDIRE IN FILIALE
008800000724     C                   if        vlrpoi <> *zeros
008900000724     C*
009000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009200000724     C     vlrpoi        chain     azorg01l
009300000724     C                   if        %found
009400000616     C                   movel(p)  CMD(1)        depcmd
009500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009600000616     C*
009700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009800011113     C                   Z-ADD     150           LENGH            15 5
009900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010000000616     C                   PARM                    depcmd
010100000616     C                   PARM                    LENGH
010200000724     C*
010300000724     C                   endif
010400000724     C                   endif
010500000616     C*
010600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010700000616     C                   movel(p)  CMD(2)        depcmd
010800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010900000616     C*
011000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011100011113     C                   Z-ADD     150           LENGH            15 5
011200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011300000616     C                   PARM                    depcmd
011400000616     C                   PARM                    LENGH
011500000616     C*
011600101116     C                   if        not %open(titv2b2ps)
011700101116     C                   open      titv2b2ps
011800000616     C                   except    testdett
011900000613     C                   endif
012000000613     C*
012100100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
012200070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
012300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012400000613     C*
012500010202     C* Effettuo la chiamata al CLLE preposto
012600100525     C  N51              call(e)   'TITVVTC'
012700100525     C                   parm                    parccm
012800100525     C                   parm                    parmbr
012900100525     C                   parm      '2'           paropz
013000100525     C   51              call(e)   'TITVVBC'
013100100525     C                   parm                    parccm
013200100525     C                   parm                    parmbr
013300100525     C                   parm      '2'           paropz
013400070730     C*
013500070730     C* Effettuo lancio TISI95 solo x chiusura
013600070730     C                   CLEAR                   TISI95DS
013700070730     C                   EVAL      I95TLA = 'C'
013800070730     C                   CALL      'TISI95R'
013900070730     C                   PARM                    TISI95DS
014000010202     C*
014100101116     C                   if        %open(titv2b2ps)
014200000616     C                   except    findett
014300101116     C                   close     titv2b2ps
014400000613     C                   endif
014500000616     C*
014600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
014700011113     C                   Z-ADD     150           LENGH            15 5
014800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014900000616     C                   PARM                    CMD(3)
015000000616     C                   PARM                    LENGH
015100000616     C*
015200000801     C
015300010201     C                   seton                                        LR
015400000613
015500100310
015600100310
015700000613     C*--------------------------------------------------------
015800000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015900000613     C*--------------------------------------------------------
016000000621     C     STPR          BEGSR
016100000613     C*
016200101116     C                   if        not %open(titv2b2p)
016300101116     C                   open      titv2b2p
016400990915     C                   endif
016500990915     C*
016600990915     C                   except    riepilogo
016700990915     C*
016800101116     C                   if        %open(titv2b2p)
016900101116     C                   close     titv2b2p
017000990914     C                   endif
017100990910     C*
017200000613     C                   ENDSR
017300000613     C***
017400990908
017500000801
017600910830     C*--------------------------------------------------------
017700070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
017800910830     C*--------------------------------------------------------
017900070530     C     RWFILE        BEGSR
018000990910     C*
018100990914     C                   if        not %open(tivin00r)
018200990908     C                   open      tivin00r
018300990914     C                   endif
018400070530     C*
018500100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
018600100525     C                   exsr      prevasx
018700010201     C*
018800010202     C                   if        chkcall = '0'
018900010202     C*
019000100525     C                   if        not %open(fivabwwr)
019100100525     C                   open      fivabwwr
019200100525     C                   endif
019300100525     C*
019400021113     C                   if        not %open(fivatwwr)
019500021113     C                   open      fivatwwr
019600010201     C                   endif
019700080117     C*
019800080117     C                   EXSR      INZVAR
019900080117     C                   EXSR      DEFCAM
020000990910     C*
020100010201     C                   clear                   §CTROKVB          5 0
020200020305     C                   clear                   §CTROKVT          5 0
020300000801     C                   clear                   §CTRMO            5 0
020400000801     C                   clear                   §CTRNO            5 0
020500990910     C*
020600020725     C/EXEC SQL
020700020725     C+ declare C1 cursor for select
020800101117     C+ vindta, vinflg, substr(vindta, 25, 20) as sped, rrn(tivin00r)
020900060223     C+ from tivin00r
021000101117     C+ order by sped, substr(vindta, 25, 20) desc
021100020725     C+ for read only
021200020725     C/END-EXEC
021300020725     C
021400020725     C/EXEC SQL
021500020725     C+ open C1
021600020725     C/END-EXEC
021700020725     C
021800020725     C/EXEC SQL
021900070530     C+ Fetch C1 into :INPUT_DS
022000020725     C/END-EXEC
022100020725     C*
022200020725     C                   dow       sqlcod = *zeros
022300990913     C*
022400100310     C                   if        vindta > *blanks
022500000613     C                   add       1             rrnum
022600000801     C*
022700020725     C                   if        vinflg = *blanks
022800020725     C                             or vinflg = '0'
022900020725     C                             or vinflg = '2'
023000000801     C*
023100020725     C                   clear                   x_vinmsg
023200020725     C                   eval      x_vinflg = '1'
023300010305     C*
023400010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
023500101117     C                   EVAL      curspe=%trim(%subst(vindta:25:20))
023600010305     C*
023700100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
023800080923     C                   if        *in50 = *off
023900071003     C                   exsr      impvab
024000100525     C                   exsr      wrivab
024100100127     C                   exsr      wrivat_a                                     => carico VAT
024200071003     C                   exsr      wrivat_b                                     => carico VAT
024300100525     C   51              exsr      wrivat_e                                     => carico VAT
024400071003     C                   else
024500080923     C*
024600071009     C                   if        wDISK = 'D'
024700091223     C                   exsr      repNSP
024800071009     C                   exsr      impvab
024900100412     C                   exsr      wrivab
025000100127     C                   exsr      wrivat_a                                     => carico VAT
025100071009     C                   exsr      wrivat_b                                     => carico VAT
025200071009     C                   exsr      wrivat_e                                     => carico VAT
025300071009     C                   else
025400071009     C*
025500010305     C                   if        depspe = *blanks                             => 1° giro
025600010305     C                   eval      depspe = curspe                              => memorizz. spediz
025700080117     C                   clear                   tivinds
025800091223     C                   exsr      repNSP
025900020305     C                   exsr      impvab
026000100127     C                   exsr      wrivat_a                                     => carico VAT
026100071003     C                   exsr      wrivat_b                                     => carico VAT
026200071003     C   50              exsr      wrivat_e                                     => carico VAT
026300010305     C                   else
026400020725     C                   if        depspe <> curspe                             => rottura di spediz
026500010305     C                   eval      depspe = curspe                              => memorizz. spediz
026600100412     C                   exsr      wrivab
026700080117     C                   clear                   tivinds
026800091223     C                   exsr      repNSP
026900020305     C                   exsr      impvab
027000100127     C                   exsr      wrivat_a                                     => carico VAT
027100071003     C                   exsr      wrivat_b                                     => carico VAT
027200071003     C   50              exsr      wrivat_e                                     => carico VAT
027300020305     C                   else                                                   => x stessa spediz
027400100401     C*
027500100401     C* Se nn richiesta esclusione spedizioni "duplicate"
027600100928     C                   select
027700100928     C                   when      wDUPREC = ' '
027800020305     C                   exsr      impvab
027900100127     C                   exsr      wrivat_a                                     => carico VAT
028000071003     C                   exsr      wrivat_b                                     => carico VAT
028100071003     C   50              exsr      wrivat_e                                     => carico VAT
028200100928     C                   when      wDUPREC = 'C'
028300100928     C   50              exsr      wrivat_e                                     => carico VAT
028400100928     C                   endsl
028500010305     C                   endif
028600010305     C                   endif
028700010305     C                   endif
028800071003     C                   endif
028900071009     C                   endif
029000000905     C*
029100000905     C                   else
029200020725     C                   eval      x_vinflg = '1'
029300000905     C                   endif
029400000905     C*
029500020725     C     VINRRN        chain     tivin000
029600020725     C                   if        %found(tivin00r)
029700020725     C                   eval      y_vinflg = x_vinflg
029800020725     C                   eval      y_vinmsg = x_vinmsg
029900020725     C                   update    tivin000
030000020725     C                   endif
030100020725     C*
030200020725     C/EXEC SQL
030300070530     C+ Fetch C1 into :INPUT_DS
030400020725     C/END-EXEC
030500020725     C*
030600020725     C                   enddo
030700020725     C*
030800020725     C/EXEC SQL
030900020725     C+ close C1
031000020725     C/END-EXEC
031100000905     C*
031200020305     C* Scarico i VAB rimasti "in sospeso"
031300071009     C                   if        wDISK = 'C'
031400100412     C                   exsr      wrivab
031500071009     C                   endif
031600010202     C*
031700010202     C                   endif
031800990910
031900990910     C* Se non ci sono record con errori ...
032000000710     C                   if        §ctrno = 0
032100990910     C* ... restituisco esito OK.
032200990921     C                   eval      wrkesito = '0'
032300990910     C                   else
032400100525     C                   if        §ctrokvb > 0 OR
032500100525     C                             §ctrokvt > 0
032600990921     C                   eval      wrkesito = '1'
032700000710     C                   else
032800000710     C                   eval      wrkesito = '2'
032900990910     C                   endif
033000000710     C                   endif
033100990910     C*
033200990914     C                   if        %open(tivin00r)
033300990908     C                   close     tivin00r
033400990914     C                   endif
033500021113     C                   if        %open(fivabwwr)
033600021113     C                   close     fivabwwr
033700990914     C                   endif
033800021113     C                   if        %open(fivatwwr)
033900021113     C                   close     fivatwwr
034000010201     C                   endif
034100990910     C*
034200100525     C                   if        §ctrokvb > 0 OR
034300100525     C                             §ctrokvt > 0
034400000724     C                             and vlrpoi <> *zeros
034500010202     C                   exsr      invio
034600990920     C                   endif
034700990920     C*
034800910830     C                   ENDSR
034900000613     C***
035000100412
035100100412
035200010305
035300010305     C*----------------------------------------------------*
035400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
035500010305     C*----------------------------------------------------*
035600020305     C     WRIVAB        BEGSR
035700100412     C*
035800100412     C* Gestisco l'eventuale rottura x numero spedizione
035900100412     C                   if        VABRMA = *blanks
036000100412     C                   movel(P)  VABRMN        VABRMA
036100100412     C                   endif
036200100412     C*
036300100525     C                   if        *in51 = *off and *in31 = *off
036400100525     C                   write     fivab000                                     => scarico il VAB
036500100525     C                   endif
036600010305     C*
036700010305     C                   ENDSR
036800990920
036900000801     C*----------------------------------------------------*
037000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
037100000801     C*----------------------------------------------------*
037200010201     C     INZVAR        BEGSR
037300000801     C*
037400010201     C                   Z-ADD     *zeros        Num5_0
037500020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
037600020725     C                   MOVEL     '0'           FlgCAS            1
037700000801     C*
037800000801     C                   ENDSR
037900000801     C*----------------------------------------------------*
038000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
038100000801     C*----------------------------------------------------*
038200020904     C     DEFCAM        BEGSR
038300080923     C*
038400100928     C                   SETOFF                                       505160
038500000801     C*
038600021113     C                   CLEAR                   FIVAB000
038700021113     C                   CLEAR                   FIVAT000
038800070730     C* Imposto i valori di default...
038900101116     C                   EVAL      VABCCM = 2493817
039000101116     C                   EVAL      VATCCM = 2493817
039100101116     C                   EVAL      VABLNP = 249
039200101116     C                   EVAL      VATLNP = 249
039300100928     C                   EVAL      VABCTR = 000
039400100928     C                   EVAL      VABCTM = '7Q'
039500100525     C                   EVAL      VABCBO = '1'
039600070222     C* ... e poi verifico se sono stati passati come parametri
039700070222     C                   IF        vlrppt > *blanks
039800100525     C*
039900070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
040000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
040100070222     C                   EXSR      CHKNUM
040200070222     C                   IF        PiInt=*on
040300070222     C                   Z-ADD     PiVal         VABCCM
040400070222     C                   Z-ADD     PiVal         VATCCM
040500070222     C                   ENDIF
040600070222     C                   ENDIF
040700070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
040800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
040900070222     C                   EXSR      CHKNUM
041000070222     C                   IF        PiInt=*on
041100070222     C                   Z-ADD     PiVal         VABLNP
041200070222     C                   Z-ADD     PiVal         VATLNP
041300070222     C                   ENDIF
041400070222     C                   ENDIF
041500070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
041600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
041700070222     C                   EXSR      CHKNUM
041800070222     C                   IF        PiInt=*on
041900070222     C                   Z-ADD     PiVal         VABCTR
042000070222     C                   ENDIF
042100070928     C                   ENDIF
042200071009     C                   MOVEL     *blanks       wDISK             1
042300071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
042400071009     C                   IF        wDISK <> *blanks
042500070928     C                   SETON                                        50
042600070222     C                   ENDIF
042700100525     C                   IF        wDISK  = 'T'
042800100525     C                   SETOFF                                       50
042900100525     C                   SETON                                        51
043000100525     C                   ENDIF
043100100401     C                   MOVEL     *blanks       wDUPREC           1
043200100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
043300100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
043400100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
043500100525     C                   ENDIF
043600100525     C*
043700070222     C                   ENDIF
043800071009     C*
043900071009     C   50              EVAL      VABCTM = '7Q'
044000000801     C*
044100000801     C                   ENDSR
044200091223     C*----------------------------------------------------*
044300091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
044400091223     C*----------------------------------------------------*
044500091223     C     REPNSP        BEGSR
044600091223     C*
044700091223     C                   EXSR      INZVAR
044800091223     C                   EXSR      DEFCAM
044900100928     C*
045000100928     C                   SETON                                        60
045100091223     C*
045200091223     C* NSP => Stacco un numeratore da AZNUM
045300091223     C                   clear                   TRUL33DS
045400091223     C                   eval      I33OPE = *zeros
045500091223     C                   eval      I33CNU = 302
045600091223     C                   eval      I33NUM = 1
045700091223     C                   movel     TRUL33DS      KPJBU
045800091223     C                   call      'TRUL33R'
045900091223     C                   parm                    KPJBA
046000091223     C                   movel     KPJBU         TRUL33DS
046100091223     C                   if        O33ERR = *zeros
046200091223     C                   z-add     O33NRF        VABNSP
046300091223     C                   z-add     O33NRF        VATNSP
046400091223     C                   else
046500091223     C                   SETON                                        31
046600091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046700091223     C                             + ' ' + 'VABNSP VATNSP'
046800091223     C                   endif
046900091223     C*
047000091223     C                   ENDSR
047100000801     C*----------------------------------------------------*
047200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
047300000801     C*----------------------------------------------------*
047400100730     C     IMPVAB        BEGSR
047500070530     C*
047600070530     C                   SETOFF                                       3132
047700070928     C*
047800070928     C                   MOVE(P)   vlrpoi        VABFGS
047900070928     C                   MOVE(P)   vlrpoi        VATFGS
048000070928     C*
048100070928     C                   MOVEL     datcor        VABAAS
048200070928     C                   MOVEL     datcor        VATAAS
048300070928     C                   MOVE      datcor        VABMGS
048400100729     C*
048500100729     C* Reperimento campi ALFA
048600101117     C                   EVAL      VABRSD=%trim(%subst(vindta:510:35))
048700101230     C***                EVAL      VABRD2=%trim(%subst(vindta:545:30))
048800101230     C                   EVAL      VABIND=%trim(%subst(vindta:545:30))
048900101229     C***                EVAL      VABIND=%trim(%subst(vindta:575:30))
049000101122     C                   EVAL      VABCAD=%trim(%subst(vindta:635:9))
049100101117     C                   EVAL      VABLOD=%trim(%subst(vindta:644:30))
049200101117     C                   EVAL      VABNZD=%trim(%subst(vindta:674:3))
049300101117     C                   IF        VABNZD='I'   or
049400101117     C                             VABNZD='IT'  or
049500101117     C                             VABNZD='ITA'
049600101117     C                   EVAL      VABNZD=*blanks
049700101117     C                   ELSE
049800101117     C                   EVAL      VABCTR=090
049900101117     C                   ENDIF
050000101117     C                   EVAL      VABRMA=%trim(%subst(vindta:1:24))
050100101117     C                   EVAL      VABNOT=%trim(%subst(vindta:833:35))
050200101117     C                   EVAL      VABNT2=%trim(%subst(vindta:833+35:35))
050300101117     C                   EVAL      VABRMO=%trim(%subst(vindta:54:35))
050400101028     C*
050500101117     C                   IF        %subst(vindta:784:1)='R'
050600101117     C                   EVAL      VABCBO = '2'
050700101117     C                   ELSE
050800101117     C                   EVAL      VABCBO = '1'
050900101117     C                   ENDIF
051000100729     C*
051100100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
051200100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
051300100729     C     '@':'A'       XLATE     VABRSD        VABRSD
051400100729     C* ==
051500100729     C*
051600100729     C* Reperimento campi NUMERICI
051700100729     C                   MOVEL     DATCOR        VABAAS
051800100729     C                   MOVE      DATCOR        VABMGS
051900100730     C* RMN
052000101117     C                   EVAL      PiStr=%trim(%subst(vindta:25:20))
052100100730     C                   EXSR      CHKNUM
052200100730     C                   IF        PiInt=*on
052300100730     C                   Z-ADD     PiVal         VABRMN
052400100928     C  N60              Z-ADD     PiVal         VABNSP
052500100928     C  N60              Z-ADD     PiVal         VATNSP
052600100730     C                   ELSE
052700100730     C                   SETON                                        32
052800100730     C                   Z-ADD     1             VABRMN
052900100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053000100928     C                             + ' ' + 'VABNSP VATNSP VABRMN'
053100100730     C                   ENDIF
053200101117     C* CCM
053300101117     C                   EVAL      PiStr=%trim(%subst(vindta:45:9))
053400101117     C                   EXSR      CHKNUM
053500101117     C                   IF        PiInt=*on
053600101117     C                   Z-ADD     PiVal         VABCCM
053700101229     C                   Z-ADD     PiVal         VATCCM
053800101117     C                   ELSE
053900101117     C                   SETON                                        32
054000101117     C                   Z-ADD     *zeros        VABCCM
054100101229     C                   Z-ADD     *zeros        VATCCM
054200101117     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054300101229     C                             + ' ' + 'VABCCM VATCCM'
054400101117     C                   ENDIF
054500100729     C* NCL
054600101117     C                   EVAL      PiStr=%trim(%subst(vindta:787:4))
054700100928     C                   EXSR      CHKNUM
054800100928     C                   IF        PiInt=*on
054900100928     C                   Z-ADD     PiVal         VABNCL
055000100928     C                   ELSE
055100100928     C                   SETON                                        32
055200100928     C                   Z-ADD     *zeros        VABNCL
055300100928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055400100928     C                             + ' ' + 'VABNCL'
055500100928     C                   ENDIF
055600100729     C* PKB
055700101117     C                   EVAL      PiStr=%trim(%subst(vindta:791:7))
055800100729     C                   EXSR      CHKNUM
055900100729     C                   IF        PiNum=*on
056000101117     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
056100100729     C                   Z-ADD(H)  PiVal         VABPKB
056200100729     C                   ELSE
056300100729     C                   SETON                                        32
056400100729     C                   Z-ADD     *zeros        VABPKB
056500100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056600100729     C                             + ' ' + 'VABPKB'
056700100729     C                   ENDIF
056800101117     C* VLB
056900101117     C                   EVAL      PiStr=%trim(%subst(vindta:798:6))
057000101117     C                   EXSR      CHKNUM
057100101117     C                   IF        PiNum=*on
057200101117     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
057300101117     C                   Z-ADD(H)  PiVal         VABVLB
057400101117     C                   ELSE
057500101117     C                   SETON                                        32
057600101117     C                   Z-ADD     *zeros        VABVLB
057700101117     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
057800101117     C                             + ' ' + 'VABVLB'
057900101117     C                   ENDIF
058000100729     C* CAS
058100101117     C                   IF        %subst(vindta:940:13)<> *blanks AND
058200101117     C                             %subst(vindta:940:13)<> *zeros
058300100729     C                   EVAL      FlgCAS = '1'
058400100729     C                   EVAL      VABVCA = 'EUR'
058500101117     C                   EVAL      VABTIC = %trim(%subst(vindta:781:3))
058600101117     C                   EVAL      PiStr=%trim(%subst(vindta:940:13))
058700100729     C                   EXSR      CHKNUM
058800100729     C                   IF        PiNum=*on
058900100928     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
059000100928     C                   Z-ADD(H)  PiVal         VABCAS
059100100729     C                   ELSE
059200100729     C                   SETON                                        32
059300100729     C                   Z-ADD     *zeros        VABCAS
059400100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
059500100729     C                             + ' ' + 'VABCAS'
059600100729     C                   ENDIF
059700100729     C                   ENDIF
059800101117     C* IAS
059900101117     C                   IF        %subst(vindta:957:13)<> *blanks AND
060000101117     C                             %subst(vindta:957:13)<> *zeros
060100101117     C                   EVAL      VABVAS = 'EUR'
060200101117     C                   EVAL      PiStr=%trim(%subst(vindta:957:13))
060300101117     C                   EXSR      CHKNUM
060400101117     C                   IF        PiNum=*on
060500101117     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
060600101117     C                   Z-ADD(H)  PiVal         VABIAS
060700101117     C                   ELSE
060800101117     C                   SETON                                        32
060900101117     C                   Z-ADD     *zeros        VABIAS
061000101117     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
061100101117     C                             + ' ' + 'VABIAS'
061200101117     C                   ENDIF
061300101117     C                   ENDIF
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
066100101117     C                   eval      VATTRC = 'A'
066200101117     C                   eval      VATNOT = %trim(%subst(vindta:697:22))
066300101117     C                   exsr      wrivat
066400100127     C*
066500100127     C                   ENDSR
066600010201     C*----------------------------------------------------*
066700071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
066800010201     C*----------------------------------------------------*
066900071003     C     WRIVAT_B      BEGSR
067000010201     C*
067100021113     C* Valorizzo l buffer di scrittura del FIVAT
067200070928     C* - TIPO RECORD "B"
067300101117     C                   eval      VATTRC = 'B'
067400101117     C                   eval      VATNOT = %trim(%subst(vindta:726:12))
067500101117     C                   exsr      wrivat
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
068700101122     C*
068800101122     C                   Z-ADD     1             wCollo            4 0
068900101122     C                   MOVEL     *zeros        wColloAlfa1      12
069000101122     C*
069100101122     C                   EVAL      wColloAlfa1=%subst(vindta:738:12)
069200101122     C*
069300101122     C* Genero tanti "CHI SONO" quanti sono i colli della spedizione
069400101122     C                   dow       wCollo <= VABNCL and wCollo < 99
069500101122     C                   eval      VATNOT = %trim(wColloAlfa1) +
069600101122     C                                      %trim(%editc(wCollo:'X'))
069700101122     C                   exsr      wrivat                                       => scarico VAT
069800101122     C                   add       1             wCollo
069900101122     C                   enddo
070000101122     C*
070100071003     C*
070200071003     C                   ENDSR
070300100127     C*----------------------------------------------------*
070400100127     C*  SCARICO BUFFER FIVAT
070500100127     C*----------------------------------------------------*
070600100127     C     WRIVAT        BEGSR
070700100127     C*
070800100127     C                   if        vatNOT <> *blanks
070900100127     C                   ADD       1             §CTROKVT
071000100127     C                   write     FIVAT000
071100100127     C                   endif
071200100127     C*
071300100127     C                   ENDSR
071400020904
071500020904
071600010202     C*----------------------------------------------------*
071700100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
071800010202     C*----------------------------------------------------*
071900100525     C     PREVASX       BEGSR
072000100525     C*
072100021113     C* Compongo il nome del membro da dare al FIVATWWR
072200010202     C                   eval      parmbr = vlrhdl
072300010202     C                   movel     'M'           parmbr
072400070530     C                   eval      parccm = %subst(vlrKSC:2:7)
072500010202     C                   eval      paropz = '1'
072600100525     C*
072700010202     C* Effettuo la chiamata al CLLE preposto
072800100525     C  N51              call(e)   'TITVVTC'
072900010202     C                   parm                    parccm
073000010202     C                   parm                    parmbr
073100010202     C                   parm                    paropz
073200100525     C   51              call(e)   'TITVVBC'
073300100525     C                   parm                    parccm
073400100525     C                   parm                    parmbr
073500100525     C                   parm                    paropz
073600100525     C*
073700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
073800010202     C                   if        %error
073900010202     C                   movel     '1'           chkcall
074000010202     C                   else
074100010202     C                   movel     '0'           chkcall
074200010202     C                   endif
074300010202     C*
074400010202     C                   ENDSR
074500000801     C*----------------------------------------------------*
074600000801     C*  CONTROLLO NUMERICITA' CAMPI
074700000801     C*----------------------------------------------------*
074800000801     C     CHKNUM        BEGSR
074900081113     C*
075000081113     C                   IF        PiDecChr = *blanks
075100100503     C                   EVAL      PiDecChr = '.'
075200081113     C                   ENDIF
075300091223     C*
075400081113     C                   callp(e)  UBISNUM_Check(PiStr
075500081113     C                                          :PiDecChr
075600081113     C                                          :PiVal
075700081113     C                                          :PiNum
075800081113     C                                          :PiInt)
075900081113     C*
076000000801     C                   IF        %error
076100000801     C                   EVAL      PiInt=*off
076200000801     C                   ENDIF
076300000801     C*
076400000801     C                   ENDSR
076500000801     C***
076600011113
076700011113
076800000801
076900000801
077000990920      /TITLE Invio dei dati al punto operativo.
077100010202     C     invio         BEGSR
077200990920     C*
077300021113     C* 1° invio FIVAT
077400010201     C                   reset                   dscmz
077500010201     C                   move      vlrpoi        cmzdst
077600021113     C                   eval      cmzfld = 'FIVATWWR'
077700010201     C                   eval      cmzmbd = vlrhdl
077800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
077900021009     C***                if        prmfir = *blanks
078000021113     C                   eval      cmzfla = 'FIVAT00F'
078100021113     C                   eval      cmzmba = 'FIVAT00F'
078200021009     C***                else
078300021009     C***                eval      cmzfla = prmfir
078400021009     C***                eval      cmzmba = prmfir
078500021009     C***                endif
078600010201     C                   eval      cmznrr = *zeros
078700020305     C                   move      §ctrokvt      cmznrr
078800021018     C                   eval      cmzlba = vlrfl1
078900010201     C                   call(e)   'TIS711C'
079000010201     C                   parm                    dscmz
079100010201     C                   parm      *blanks       esito
079200010205     C                   if        %error
079300010205     C                             or cmzerr = '1'
079400010205     C                             or esito  = '1'
079500010205     C                   eval      wrkesito = '3'
079600010205     C                   else
079700010201     C*
079800021113     C* 2° invio FIVAB
079900100525     C                   if        *in51 = *off
080000010201     C                   reset                   dscmz
080100010201     C                   move      vlrpoi        cmzdst
080200010201     C                   eval      cmzfld = vlrfou
080300010201     C                   eval      cmzmbd = vlrhdl
080400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
080500021009     C***                if        prmfir = *blanks
080600021113     C                   eval      cmzfla = 'FIVAB00F'
080700021113     C                   eval      cmzmba = 'FIVAB00F'
080800021009     C***                else
080900021009     C***                eval      cmzfla = prmfir
081000021009     C***                eval      cmzmba = prmfir
081100021009     C***                endif
081200010201     C                   eval      cmznrr = *zeros
081300010201     C                   move      §ctrokvb      cmznrr
081400021018     C                   eval      cmzlba = vlrfl1
081500010201     C                   call(e)   'TIS711C'
081600010201     C                   parm                    dscmz
081700010201     C                   parm      *blanks       esito
081800010201     C                   if        %error
081900010201     C                             or cmzerr = '1'
082000010201     C                             or esito  = '1'
082100010201     C                   eval      wrkesito = '3'
082200010201     C                   endif
082300100525     C                   endif
082400010205     C                   endif
082500990920     C*
082600000613     C                   ENDSR
082700000613     C***
082800070411
082900070411     C     *pssr         BEGSR
083000070411     C*
083100070411     C                   if        %open(tivin00r)
083200070411     C                   close     tivin00r
083300070411     C                   endif
083400070411     C                   if        %open(fivabwwr)
083500070411     C                   close     fivabwwr
083600070411     C                   endif
083700070411     C                   if        %open(fivatwwr)
083800070411     C                   close     fivatwwr
083900070411     C                   endif
084000070411     C*
084100070411     C* Effettuo la chiamata al CLLE preposto
084200100525     C  N51              call(e)   'TITVVTC'
084300100525     C                   parm                    parccm
084400100525     C                   parm                    parmbr
084500100525     C                   parm      '2'           paropz
084600100525     C   51              call(e)   'TITVVBC'
084700100525     C                   parm                    parccm
084800100525     C                   parm                    parmbr
084900100525     C                   parm      '2'           paropz
085000070411     C*
085100070411     C                   eval      wrkesito = '2'
085200070411     C*
085300070411     C                   seton                                        LR
085400070411     C*
085500070411     C                   ENDSR     '*CANCL'
085600070411     C***
085700070411
085800990910
085900000613     C     *inzsr        BEGSR
086000990910     C*
086100990910     C     *entry        plist
086200990920     C                   parm                    tivlrds
086300990921     C                   parm      wrkesito      esito
086400000724     C                   parm                    prmlit
086500000710     C                   parm                    prmfir
086600000613     C*
086700000830     C* CALCOLA LA DATA CORRENTE
086800091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
086900091223     C                   eval      datcor = %dec(%date() : *ISO)
087000000830     C*
087100000613     C                   ENDSR
087200000613     C***
087300990908
087400101116     Otitv2b2p  E            riepilogo         2
087500990915     O                                              'Upload via Internet'
087600990915     O                                           +1 'Traduzione TIVIN00R -'
087700021113     O                                           +1 'FIVABWWR/FIVATWWR'
087800010201     O                                           +1 'Report testate bolle'
087900990915     O          E            riepilogo   2
088000990915     O                       wrkdata
088100990915     O                       wrkora              +1
088200990915     O                       procname            +1
088300990915     O          E            riepilogo   2
088400990915     O                                              'Cliente..................:'
088500990915     O                       VABCCM        z     +1
088600990915     O          E            riepilogo   2
088700990920     O                                              'Riferimento Strategi.....:'
088800990920     O                       vlrhdl              +1
088900990915     O          E            riepilogo   2
089000990915     O                                              'Giusti...................:'
089100010201     O                       §CTROKVB      2   +  1
089200100525     O                       §CTROKVT      2   +  1
089300990915     O          E            riepilogo   2
089400990915     O                                              'Sbagliati e corretti.....:'
089500971022     O                       §CTRMO        2   +  1
089600990915     O          E            riepilogo   2
089700990915     O                                              'Sbagliati e scartati.....:'
089800971022     O                       §CTRNO        2   +  1
089900000613
090000101116     Otitv2b2ps E            testdett          2
090100000613     O                                              'Upload via Internet'
090200000613     O                                           +1 'Traduzione TIVIN00R -'
090300021113     O                                           +1 'FIVABWWR/FIVATWWR'
090400010201     O                                           +1 'Report testate bolle'
090500000616     O          E            testdett    3
090600000613     O                                           +2 'N° rec'
090700000613     O                                           +3 'Anteprima contenuto'
090800000616     O          E            rigadett    2
090900000613     O                       rrnum               +2
091000000621     O                       recko               +3
091100000616     O          E            findett     2
091200000613     O                       wrkdata
091300000613     O                       wrkora              +1
091400000613     O                       procname            +1
091500000616     O          E            findett     2
091600000613     O                                              'Cliente..................:'
091700000613     O                       VABCCM        z     +1
091800000616     O          E            findett     2
091900000613     O                                              'Riferimento Strategi.....:'
092000000613     O                       vlrhdl              +1
092100000616     O          E            findett     2
092200000613     O                                              'Giusti...................:'
092300010201     O                       §CTROKVB      2   +  1
092400100525     O                       §CTROKVT      2   +  1
092500000616     O          E            findett     2
092600000613     O                                              'Sbagliati e corretti.....:'
092700000613     O                       §CTRMO        2   +  1
092800000616     O          E            findett     2
092900000613     O                                              'Sbagliati e scartati.....:'
093000000613     O                       §CTRNO        2   +  1
093100000512** CMD - COMANDI CL
093200101116OVRPRTF FILE(TITV2B2P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
093300101116OVRPRTF FILE(TITV2B2PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
093400101116DLTOVR FILE(TITV2B2P TITV2B2PS) LVL(*)
093500000512
093600000512
