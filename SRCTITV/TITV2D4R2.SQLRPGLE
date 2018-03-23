000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400120420     Ftabel00f  if   e           k disk
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600100412     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800120420
000900120420     D*------------
001000120420     D* SCHIERE
001100120420     D*------------
001200120420     D skNAZISO        S              3    DIM(1000)
001300120420     D skNAZBAR        S              3    DIM(1000)
001400990908
001500000801     D*----------------------------------------------------
001600000801     D* DICHIARAZIOINE VARIABILI DI WRK
001700000801     D*----------------------------------------------------
001800990920     D dscmz         e ds                  inz
001900990910     D psds           sds
002000990910     D  procname         *PROC
002100990920     D tivlrds       e ds                  extname(tivlr00f)
002200120420     D ds15          e ds
002300070730     D tisi95ds      e ds
002400990910     D esito           s              1
002500000724     D prmlit          s             10
002600000710     D prmfir          s             10
002700990921     D wrkesito        s                   like(esito)
002800000613     D rrnum           s              6  0 INZ(*zeros)
002900110407     D depspe          s             15    INZ(*blanks)
003000110407     D curspe          s             15    INZ(*blanks)
003100010202     D parccm          s              8    INZ(*blanks)
003200010202     D parmbr          s             10    INZ(*blanks)
003300010202     D paropz          s              1    INZ(*blanks)
003400010202     D chkcall         s              1    INZ(*blanks)
003500080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003600091223     D Num5_0          s              5  0
003700120420     D jNAZ            s              5  0 INZ(*zeros)
003800020725
003900020725     D*------------------
004000020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004100020725     D*------------------
004200070530     D INPUT_DS        DS                  INZ
004300100729     D  VINDTA                     2048
004400100729     D  VINFLG                        1
004500110407     D  VINSPE                       15
004600100729     D  VINRRN                        8  0
004700020725     D*
004800081113
004900091223     D*------------------
005000091223     D* DS REPERIMENTO NUMERATORE
005100091223     D*------------------
005200100309     D trul33ds      e ds                  inz
005300091223     D*------------------
005400091223     D* DS ARCHITETTURA
005500091223     D*------------------
005600091223     D kpjba         e ds                  inz
005700091223
005800101119
005900101119     D*-------------------
006000101119     D* COSTANTI
006100101119     D*-------------------
006200101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
006300101119     D                                     èéù')
006400101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
006500101119     D                                     EEU')
006600081113
006700081113     D*------------------
006800081113     D* LINKING A DEFINIZIONI ESTERNE
006900081113     D*------------------
007000081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007100090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007200081113
007300990908
007400010201
007500010201
007600080117     C*
007700080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007800080117     C
007900080117     C/EXEC SQL
008000080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008100080117     C/END-EXEC
008200080117     C
008300000913     C                   reset                   rrnum
008400990921     C                   reset                   esito
008500990921     C                   reset                   wrkesito
008600000613     C*
008700120420     C                   EXSR      CARTAB                                       CARICA TABELLE
008800120515     C                   EXSR      INZVAR
008900100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
009000070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
009100000613     C*
009200010202     C* Effettuo la chiamata al CLLE preposto
009300100525     C  N51              call(e)   'TITVVTC'
009400100525     C                   parm                    parccm
009500100525     C                   parm                    parmbr
009600100525     C                   parm      '2'           paropz
009700100525     C   51              call(e)   'TITVVBC'
009800100525     C                   parm                    parccm
009900100525     C                   parm                    parmbr
010000100525     C                   parm      '2'           paropz
010100070730     C*
010200070730     C* Effettuo lancio TISI95 solo x chiusura
010300070730     C                   CLEAR                   TISI95DS
010400070730     C                   EVAL      I95TLA = 'C'
010500070730     C                   CALL      'TISI95R'
010600070730     C                   PARM                    TISI95DS
010700000616     C*
010800000801     C
010900010201     C                   seton                                        LR
011000120420
011100120420     C*--------------------------------------------------------
011200120420     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
011300120420     C*--------------------------------------------------------
011400120420     C     CARTAB        BEGSR
011500120420     C*
011600120420     C* TABELLA '15' - NAZIONI
011700120420     C                   clear                   skNAZISO
011800120420     C                   clear                   skNAZBAR
011900120420     C                   eval      tblKUT = 1
012000120420     C                   eval      tblCOD = '15'
012100120420     C     KEYtabP       setll     tabel00f
012200120420     C     KEYtabP       reade     tabel00f
012300120420     C                   dow       not %eof(tabel00f)
012400120420     C                   if        tblFLG = *blanks
012500120420     C                   movel(p)  tblUNI        ds15
012600120420     C                   add       1             jNAZ
012700120420     C                   eval      skNAZBAR(jNAZ) = tblKEY
012800120420     C                   eval      skNAZISO(jNAZ) = §15COD
012900120420     C                   endif
013000120420     C     KEYtabP       reade     tabel00f
013100120420     C                   enddo
013200120420     C*
013300120420     C                   ENDSR
013400120420     C***
013500990908
013600000801
013700910830     C*--------------------------------------------------------
013800070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
013900910830     C*--------------------------------------------------------
014000070530     C     RWFILE        BEGSR
014100990910     C*
014200990914     C                   if        not %open(tivin00r)
014300990908     C                   open      tivin00r
014400990914     C                   endif
014500070530     C*
014600100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
014700100525     C                   exsr      prevasx
014800010201     C*
014900010202     C                   if        chkcall = '0'
015000010202     C*
015100100525     C                   if        not %open(fivabwwr)
015200100525     C                   open      fivabwwr
015300100525     C                   endif
015400100525     C*
015500021113     C                   if        not %open(fivatwwr)
015600021113     C                   open      fivatwwr
015700010201     C                   endif
015800080117     C*
015900080117     C                   EXSR      INZVAR
016000080117     C                   EXSR      DEFCAM
016100990910     C*
016200010201     C                   clear                   §CTROKVB          5 0
016300020305     C                   clear                   §CTROKVT          5 0
016400000801     C                   clear                   §CTRMO            5 0
016500000801     C                   clear                   §CTRNO            5 0
016600990910     C*
016700020725     C/EXEC SQL
016800020725     C+ declare C1 cursor for select
016900110407     C+ vindta, vinflg, substr(vindta, 25, 15) as sped, rrn(tivin00r)
017000060223     C+ from tivin00r
017100110407     C+ order by sped, substr(vindta, 25, 15) desc
017200020725     C+ for read only
017300020725     C/END-EXEC
017400020725     C
017500020725     C/EXEC SQL
017600020725     C+ open C1
017700020725     C/END-EXEC
017800020725     C
017900020725     C/EXEC SQL
018000070530     C+ Fetch C1 into :INPUT_DS
018100020725     C/END-EXEC
018200020725     C*
018300020725     C                   dow       sqlcod = *zeros
018400990913     C*
018500100310     C                   if        vindta > *blanks
018600000613     C                   add       1             rrnum
018700000801     C*
018800020725     C                   if        vinflg = *blanks
018900020725     C                             or vinflg = '0'
019000020725     C                             or vinflg = '2'
019100000801     C*
019200020725     C                   clear                   x_vinmsg
019300020725     C                   eval      x_vinflg = '1'
019400101119     C*
019500101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
019600101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
019700010305     C*
019800010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
019900120514     C* se spedizione in altri cicuiti uso RMA
020000140721     C***                IF        %subst(vindta:390:3) <> 'DPD'
020100110407     C                   EVAL      curspe=%trim(%subst(vindta:25:15))
020200140721     C***                EXSR      DEFCAM
020300120514     C* altrimenti stacco un numeratore "qualunque" per avere un valore univoco e quindi non
020400120514     C* accorpare
020500140721     C***                ELSE
020600140721     C***                clear                   TRUL33DS
020700140721     C***                eval      I33OPE = *zeros
020800140721     C***                eval      I33CNU = 302
020900140721     C***                eval      I33NUM = 1
021000140721     C***                movel     TRUL33DS      KPJBU
021100140721     C***                call      'TRUL33R'
021200140721     C***                parm                    KPJBA
021300140721     C***                movel     KPJBU         TRUL33DS
021400140721     C***                if        O33ERR = *zeros
021500140721     C***                EVAL      CurSpe = %char(O33NRF)
021600140721     C***                else
021700140721     C***                SETON                                        31
021800140721     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
021900140721     C***                          + ' ' + 'CURSPE'
022000140721     C***                endif
022100140721     C***                ENDIF
022200010305     C*
022300100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
022400080923     C                   if        *in50 = *off
022500101119     C                   exsr      inzvar
022600101119     C                   exsr      defcam
022700071003     C                   exsr      impvab
022800100525     C                   exsr      wrivab
022900100127     C                   exsr      wrivat_a                                     => carico VAT
023000071003     C                   exsr      wrivat_b                                     => carico VAT
023100100525     C   51              exsr      wrivat_e                                     => carico VAT
023200071003     C                   else
023300080923     C*
023400071009     C                   if        wDISK = 'D'
023500091223     C                   exsr      repNSP
023600071009     C                   exsr      impvab
023700100412     C                   exsr      wrivab
023800100127     C                   exsr      wrivat_a                                     => carico VAT
023900071009     C                   exsr      wrivat_b                                     => carico VAT
024000071009     C                   exsr      wrivat_e                                     => carico VAT
024100071009     C                   else
024200071009     C*
024300010305     C                   if        depspe = *blanks                             => 1° giro
024400010305     C                   eval      depspe = curspe                              => memorizz. spediz
024500080117     C                   clear                   tivinds
024600091223     C                   exsr      repNSP
024700020305     C                   exsr      impvab
024800100127     C                   exsr      wrivat_a                                     => carico VAT
024900071003     C                   exsr      wrivat_b                                     => carico VAT
025000071003     C   50              exsr      wrivat_e                                     => carico VAT
025100010305     C                   else
025200020725     C                   if        depspe <> curspe                             => rottura di spediz
025300010305     C                   eval      depspe = curspe                              => memorizz. spediz
025400100412     C                   exsr      wrivab
025500080117     C                   clear                   tivinds
025600091223     C                   exsr      repNSP
025700020305     C                   exsr      impvab
025800100127     C                   exsr      wrivat_a                                     => carico VAT
025900071003     C                   exsr      wrivat_b                                     => carico VAT
026000071003     C   50              exsr      wrivat_e                                     => carico VAT
026100020305     C                   else                                                   => x stessa spediz
026200100401     C*
026300100401     C* Se nn richiesta esclusione spedizioni "duplicate"
026400100928     C                   select
026500100928     C                   when      wDUPREC = ' '
026600020305     C                   exsr      impvab
026700100127     C                   exsr      wrivat_a                                     => carico VAT
026800071003     C                   exsr      wrivat_b                                     => carico VAT
026900071003     C   50              exsr      wrivat_e                                     => carico VAT
027000100928     C                   when      wDUPREC = 'C'
027100100928     C   50              exsr      wrivat_e                                     => carico VAT
027200100928     C                   endsl
027300010305     C                   endif
027400010305     C                   endif
027500010305     C                   endif
027600071003     C                   endif
027700110201     C*
027800071009     C                   endif
027900000905     C*
028000000905     C                   else
028100020725     C                   eval      x_vinflg = '1'
028200000905     C                   endif
028300000905     C*
028400020725     C     VINRRN        chain     tivin000
028500020725     C                   if        %found(tivin00r)
028600020725     C                   eval      y_vinflg = x_vinflg
028700020725     C                   eval      y_vinmsg = x_vinmsg
028800020725     C                   update    tivin000
028900020725     C                   endif
029000020725     C*
029100020725     C/EXEC SQL
029200070530     C+ Fetch C1 into :INPUT_DS
029300020725     C/END-EXEC
029400020725     C*
029500020725     C                   enddo
029600020725     C*
029700020725     C/EXEC SQL
029800020725     C+ close C1
029900020725     C/END-EXEC
030000000905     C*
030100020305     C* Scarico i VAB rimasti "in sospeso"
030200071009     C                   if        wDISK = 'C'
030300100412     C                   exsr      wrivab
030400071009     C                   endif
030500010202     C*
030600010202     C                   endif
030700990910
030800990910     C* Se non ci sono record con errori ...
030900000710     C                   if        §ctrno = 0
031000990910     C* ... restituisco esito OK.
031100990921     C                   eval      wrkesito = '0'
031200990910     C                   else
031300100525     C                   if        §ctrokvb > 0 OR
031400100525     C                             §ctrokvt > 0
031500990921     C                   eval      wrkesito = '1'
031600000710     C                   else
031700000710     C                   eval      wrkesito = '2'
031800990910     C                   endif
031900000710     C                   endif
032000990910     C*
032100990914     C                   if        %open(tivin00r)
032200990908     C                   close     tivin00r
032300990914     C                   endif
032400021113     C                   if        %open(fivabwwr)
032500021113     C                   close     fivabwwr
032600990914     C                   endif
032700021113     C                   if        %open(fivatwwr)
032800021113     C                   close     fivatwwr
032900010201     C                   endif
033000990910     C*
033100100525     C                   if        §ctrokvb > 0 OR
033200100525     C                             §ctrokvt > 0
033300000724     C                             and vlrpoi <> *zeros
033400010202     C                   exsr      invio
033500990920     C                   endif
033600990920     C*
033700910830     C                   ENDSR
033800000613     C***
033900100412
034000100412
034100010305
034200010305     C*----------------------------------------------------*
034300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
034400010305     C*----------------------------------------------------*
034500020305     C     WRIVAB        BEGSR
034600100412     C*
034700100412     C* Gestisco l'eventuale rottura x numero spedizione
034800100412     C                   if        VABRMA = *blanks
034900100412     C                   movel(P)  VABRMN        VABRMA
035000100412     C                   endif
035100100412     C*
035200100525     C                   if        *in51 = *off and *in31 = *off
035300100525     C                   write     fivab000                                     => scarico il VAB
035400100525     C                   endif
035500010305     C*
035600010305     C                   ENDSR
035700990920
035800000801     C*----------------------------------------------------*
035900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
036000000801     C*----------------------------------------------------*
036100010201     C     INZVAR        BEGSR
036200000801     C*
036300010201     C                   Z-ADD     *zeros        Num5_0
036400020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
036500020725     C                   MOVEL     '0'           FlgCAS            1
036600120515     C*
036700120515     C                   CLEAR                   FIVAB000
036800120515     C                   CLEAR                   FIVAT000
036900120515     C*
037000120515     C                   SETOFF                                       505160
037100000801     C*
037200000801     C                   ENDSR
037300000801     C*----------------------------------------------------*
037400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
037500000801     C*----------------------------------------------------*
037600020904     C     DEFCAM        BEGSR
037700120515     C*
037800070730     C* Imposto i valori di default...
037900110325     C                   EVAL      VABCCM = 0494115
038000110325     C                   EVAL      VATCCM = 0494115
038100110325     C                   EVAL      VABLNP = 049
038200110325     C                   EVAL      VATLNP = 049
038300110128     C                   EVAL      VABCTR = 000
038400100525     C                   EVAL      VABCBO = '1'
038500070222     C* ... e poi verifico se sono stati passati come parametri
038600070222     C                   IF        vlrppt > *blanks
038700100525     C*
038800070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
038900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
039000070222     C                   EXSR      CHKNUM
039100070222     C                   IF        PiInt=*on
039200070222     C                   Z-ADD     PiVal         VABCCM
039300070222     C                   Z-ADD     PiVal         VATCCM
039400070222     C                   ENDIF
039500070222     C                   ENDIF
039600070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
039700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
039800070222     C                   EXSR      CHKNUM
039900070222     C                   IF        PiInt=*on
040000070222     C                   Z-ADD     PiVal         VABLNP
040100070222     C                   Z-ADD     PiVal         VATLNP
040200070222     C                   ENDIF
040300070222     C                   ENDIF
040400070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
040500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
040600070222     C                   EXSR      CHKNUM
040700070222     C                   IF        PiInt=*on
040800120419     C                   Z-ADD     PiVal         VABCTR
040900070222     C                   ENDIF
041000070928     C                   ENDIF
041100071009     C                   MOVEL     *blanks       wDISK             1
041200071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
041300071009     C                   IF        wDISK <> *blanks
041400070928     C                   SETON                                        50
041500070222     C                   ENDIF
041600100525     C                   IF        wDISK  = 'T'
041700100525     C                   SETOFF                                       50
041800100525     C                   SETON                                        51
041900100525     C                   ENDIF
042000100401     C                   MOVEL     *blanks       wDUPREC           1
042100100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
042200100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
042300100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
042400100525     C                   ENDIF
042500120419     C                   IF        %subst(vlrppt:18:3) <> *blanks
042600120419     C                   EVAL      PiStr=%trim(%subst(vlrppt:18:3))
042700120419     C                   EXSR      CHKNUM
042800120419     C                   IF        PiInt=*on
042900120419     C                   Z-ADD     PiVal         wCTR_EEX          3 0
043000120419     C                   ENDIF
043100120419     C                   ENDIF
043200120419     C                   IF        %subst(vlrppt:21:3) <> *blanks
043300120419     C                   EVAL      PiStr=%trim(%subst(vlrppt:21:3))
043400120419     C                   EXSR      CHKNUM
043500120419     C                   IF        PiInt=*on
043600120419     C                   Z-ADD     PiVal         wCTR_DPD          3 0
043700120419     C                   ENDIF
043800120419     C                   ENDIF
043900100525     C*
044000070222     C                   ENDIF
044100071009     C*
044200071009     C   50              EVAL      VABCTM = '7Q'
044300000801     C*
044400000801     C                   ENDSR
044500091223     C*----------------------------------------------------*
044600091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
044700091223     C*----------------------------------------------------*
044800091223     C     REPNSP        BEGSR
044900091223     C*
045000091223     C                   EXSR      INZVAR
045100091223     C                   EXSR      DEFCAM
045200100928     C*
045300100928     C                   SETON                                        60
045400091223     C*
045500091223     C* NSP => Stacco un numeratore da AZNUM
045600091223     C                   clear                   TRUL33DS
045700091223     C                   eval      I33OPE = *zeros
045800091223     C                   eval      I33CNU = 302
045900091223     C                   eval      I33NUM = 1
046000091223     C                   movel     TRUL33DS      KPJBU
046100091223     C                   call      'TRUL33R'
046200091223     C                   parm                    KPJBA
046300091223     C                   movel     KPJBU         TRUL33DS
046400091223     C                   if        O33ERR = *zeros
046500091223     C                   z-add     O33NRF        VABNSP
046600091223     C                   z-add     O33NRF        VATNSP
046700091223     C                   else
046800091223     C                   SETON                                        31
046900091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047000091223     C                             + ' ' + 'VABNSP VATNSP'
047100091223     C                   endif
047200091223     C*
047300091223     C                   ENDSR
047400000801     C*----------------------------------------------------*
047500021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
047600000801     C*----------------------------------------------------*
047700100730     C     IMPVAB        BEGSR
047800070530     C*
047900070530     C                   SETOFF                                       3132
048000070928     C*
048100070928     C                   MOVE(P)   vlrpoi        VABFGS
048200070928     C                   MOVE(P)   vlrpoi        VATFGS
048300070928     C*
048400070928     C                   MOVEL     datcor        VABAAS
048500070928     C                   MOVEL     datcor        VATAAS
048600070928     C                   MOVE      datcor        VABMGS
048700100729     C*
048800100729     C* Reperimento campi ALFA
048900110325     C                   EVAL      VABRSD=%trim(%subst(vindta:236:40))
049000110325     C                   EVAL      VABIND=%trim(%subst(vindta:276:60))
049100110325     C                   EVAL      VABLOD=%trim(%subst(vindta:345:30))
049200110325     C                   EVAL      VABPRD=%trim(%subst(vindta:375:2))
049300110325     C                   EVAL      VABCAD=%trim(%subst(vindta:336:9))
049400110929     C                   EVAL      VABNT2='Cliente: ' +
049500110929     C                                    %trim(%subst(vindta:470:10))
049600120420     C* per la nazione consideriamo il dato "nazione diversa" (che è la ISO) e poi la
049700120420     C* sostituiamo con la nostra
049800120420     C                   EVAL      VABNZD=%trim(%subst(vindta:377:3))
049900120420     C                   Z-ADD     1             jNAZ
050000120420     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         13
050100120420     C                   IF        %found
050200120420     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
050300120420     C                   ENDIF
050400110331     C* CBO
050500110127     C                   SELECT
050600110325     C                   WHEN      %subst(vindta:60:2) = '02'
050700110325     C                   EVAL      VABCBO = '2'
050800110127     C                   ENDSL
050900110929     C* TSP
051000120420     C* dalla richiesta del POC si evince che il tipo servizio come descritto in precedenza non
051100120420     C* deve essere più valorizzato, ma la posizione va usata per valorizzare la tariffa CTR
051200110929     C                   SELECT
051300120420     C***                WHEN      %subst(vindta:390:3) = '1'
051400120420     C***                EVAL      VABTSP = 'C'
051500120420     C***                WHEN      %subst(vindta:390:3) = '2'
051600120420     C***                EVAL      VABTSP = 'H'
051700120420     C***                WHEN      %subst(vindta:390:3) = '3'
051800120420     C***                EVAL      VABTSP = 'E'
051900120420     C                   WHEN      %subst(vindta:390:3) = 'EXP'
052000120420     C                   EVAL      VABCTR = wCTR_EEX
052100120420     C                   WHEN      %subst(vindta:390:3) = 'DPD'
052200120420     C                   EVAL      VABCTR = wCTR_DPD
052300120423     C   50              EVAL      VABCTM = '7R'
052400110929     C                   ENDSL
052500110331     C* FFD
052600110331     C                   SELECT
052700110331     C                   WHEN      %subst(vindta:429:1) = '1'
052800110331     C                   EVAL      VABFFD = 'S'
052900110331     C                   ENDSL
053000110127     C*
053100110325     C                   EVAL      VABRMA=%trim(%subst(vindta:25:15))
053200100729     C*
053300100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
053400100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
053500100729     C     '@':'A'       XLATE     VABRSD        VABRSD
053600100729     C* ==
053700100729     C*
053800100729     C* Reperimento campi NUMERICI
053900100729     C                   MOVEL     DATCOR        VABAAS
054000100729     C                   MOVE      DATCOR        VABMGS
054100110127     C* NSP / RMN
054200101119     C                   IF        *IN51 = *off
054300110325     C                   EVAL      PiStr=%trim(%subst(vindta:50:10))
054400100730     C                   EXSR      CHKNUM
054500100730     C                   IF        PiInt=*on
054600100730     C                   Z-ADD     PiVal         VABRMN
054700100928     C  N60              Z-ADD     PiVal         VABNSP
054800100928     C  N60              Z-ADD     PiVal         VATNSP
054900100730     C                   ELSE
055000100730     C                   SETON                                        32
055100100730     C                   Z-ADD     1             VABRMN
055200100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055300101119     C                             + ' ' + 'VABNSP VABRMN VATNSP'
055400100730     C                   ENDIF
055500101119     C*
055600101119     C                   ELSE
055700101119     C*
055800110407     C                   EVAL      PiStr=%trim(%subst(vindta:50:10))
055900101119     C                   EXSR      CHKNUM
056000101119     C                   IF        PiInt=*on
056100101119     C  N60              Z-ADD     PiVal         VATNSP
056200101119     C                   ELSE
056300101119     C                   SETON                                        32
056400101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056500101119     C                             + ' ' + 'VATNSP'
056600101119     C                   ENDIF
056700101119     C                   ENDIF
056800110201     C* NCL
056900110325     C                   EVAL      PiStr=%trim(%subst(vindta:409:5))
057000100928     C                   EXSR      CHKNUM
057100100928     C                   IF        PiInt=*on
057200110325     C                   Z-ADD     PiVal         VABNCL
057300100928     C                   ELSE
057400101119     C  N51              SETON                                        32
057500101119     C  N51              Z-ADD     *zeros        VABNCL
057600101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
057700100928     C                             + ' ' + 'VABNCL'
057800100928     C                   ENDIF
057900100729     C* PKB
058000110325     C                   IF        %subst(vindta:414:8) <> *all'0' AND
058100110325     C                             %subst(vindta:414:8) <> *blanks
058200110325     C                   EVAL      PiStr=%trim(%subst(vindta:414:8))
058300100729     C                   EXSR      CHKNUM
058400100729     C                   IF        PiNum=*on
058500110325     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
058600110325     C                   Z-ADD(H)  PiVal         VABPKB
058700100729     C                   ELSE
058800101119     C  N51              SETON                                        32
058900101119     C  N51              Z-ADD     *zeros        VABPKB
059000101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
059100100729     C                             + ' ' + 'VABPKB'
059200100729     C                   ENDIF
059300110325     C                   ENDIF
059400110325     C* VLB
059500110325     C                   IF        %subst(vindta:422:7) <> *all'0' AND
059600110325     C                             %subst(vindta:422:7) <> *blanks
059700110325     C                   EVAL      PiStr=%trim(%subst(vindta:422:7))
059800110325     C                   EXSR      CHKNUM
059900110325     C                   IF        PiNum=*on
060000110325     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
060100110325     C                   Z-ADD(H)  PiVal         VABVLB
060200110325     C                   ELSE
060300110325     C  N51              SETON                                        32
060400110325     C  N51              Z-ADD     *zeros        VABVLB
060500110325     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
060600110325     C                             + ' ' + 'VABVLB'
060700110325     C                   ENDIF
060800110325     C                   ENDIF
060900100729     C* CAS
061000110325     C                   IF        %subst(vindta:396:13) <> *all'0' AND
061100110325     C                             %subst(vindta:396:13) <> *blanks
061200100729     C                   EVAL      FlgCAS = '1'
061300110127     C                   EVAL      VABVCA = 'EUR'
061400110928     C                   EVAL      VABTIC = 'BM'
061500110325     C                   EVAL      PiStr=%trim(%subst(vindta:396:13))
061600100729     C                   EXSR      CHKNUM
061700100729     C                   IF        PiNum=*on
061800110325     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
061900100928     C                   Z-ADD(H)  PiVal         VABCAS
062000100729     C                   ELSE
062100101119     C  N51              SETON                                        32
062200101119     C  N51              Z-ADD     *zeros        VABCAS
062300101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
062400100729     C                             + ' ' + 'VABCAS'
062500100729     C                   ENDIF
062600101119     C                   ELSE
062700101119     C                   EVAL      FlgCAS = '0'
062800100729     C                   ENDIF
062900100729     C*
063000100729     C* Se provincia nn valorizzata la reperisco
063100100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
063200100729     C                   IF        VABNZD  = *blanks AND
063300100729     C                             VABPRD  = *blanks AND
063400100729     C                             VABCAD <> *blanks
063500100729     C                   CLEAR                   TISI95DS
063600100729     C                   EVAL      I95TCN = '3'
063700100729     C                   Z-ADD     datcor        I95DAT
063800100729     C                   EVAL      I95NAR = VABNZD
063900100729     C                   EVAL      I95CAP = VABCAD
064000100729     C                   EVAL      I95LOC = VABLOD
064100100729     C                   CALL      'TISI95R'
064200100729     C                   PARM                    TISI95DS
064300100729     C                   EVAL      VABPRD = O95PRV
064400100729     C                   ENDIF
064500100729     C*
064600100729     C* Considerazioni finali su CBO/CAS
064700100729     C                   IF        FlgCAS = '1'
064800100729     C                   IF        VABCBO = '1'
064900100729     C                   EVAL      VABCBO = '4'
065000100729     C                   ENDIF
065100100729     C                   IF        VABCBO = '2'
065200100729     C                   EVAL      VABCBO = '6'
065300100729     C                   ENDIF
065400100729     C                   ENDIF
065500010202     C*
065600000801     C* Ebbene...
065700000801     C                   ADD       1             §CTRMO
065800070530     C                   IF        *in31 <> *zeros OR
065900070530     C                             *in32 <> *zeros
066000000801     C                   ADD       1             §CTRNO
066100020725     C                   EVAL      x_vinflg = '2'
066200000801     C                   ELSE
066300010201     C                   ADD       1             §CTROKVB
066400000801     C                   ENDIF
066500000801     C*
066600000801     C                   ENDSR
066700100127     C*----------------------------------------------------*
066800100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
066900100127     C*----------------------------------------------------*
067000100127     C     WRIVAT_A      BEGSR
067100100127     C*
067200100127     C* Valorizzo l buffer di scrittura del FIVAT
067300100127     C* - TIPO RECORD "A"
067400101119     C***                eval      VATTRC = 'A'
067500101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
067600101119     C***                exsr      wrivat
067700100127     C*
067800100127     C                   ENDSR
067900010201     C*----------------------------------------------------*
068000071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
068100010201     C*----------------------------------------------------*
068200071003     C     WRIVAT_B      BEGSR
068300010201     C*
068400021113     C* Valorizzo l buffer di scrittura del FIVAT
068500070928     C* - TIPO RECORD "B"
068600110325     C                   eval      VATTRC = 'B'
068700110325     C                   eval      VATNOT = %trim(%subst(vindta:206:15))
068800110325     C                   exsr      wrivat
068900010201     C*
069000010201     C                   ENDSR
069100071003     C*----------------------------------------------------*
069200071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
069300071003     C*----------------------------------------------------*
069400071003     C     WRIVAT_E      BEGSR
069500071003     C*
069600071003     C* Valorizzo l buffer di scrittura del FIVAT
069700071003     C* - TIPO RECORD "E"
069800100729     C*
069900100928     C                   eval      VATTRC = 'E'
070000110325     C                   eval      VATNOT = %trim(%subst(vindta:430:35))
070100100928     C                   exsr      wrivat
070200071003     C*
070300071003     C                   ENDSR
070400100127     C*----------------------------------------------------*
070500100127     C*  SCARICO BUFFER FIVAT
070600100127     C*----------------------------------------------------*
070700100127     C     WRIVAT        BEGSR
070800100127     C*
070900100127     C                   if        vatNOT <> *blanks
071000100127     C                   ADD       1             §CTROKVT
071100100127     C                   write     FIVAT000
071200100127     C                   endif
071300100127     C*
071400100127     C                   ENDSR
071500020904
071600020904
071700010202     C*----------------------------------------------------*
071800100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
071900010202     C*----------------------------------------------------*
072000100525     C     PREVASX       BEGSR
072100100525     C*
072200021113     C* Compongo il nome del membro da dare al FIVATWWR
072300010202     C                   eval      parmbr = vlrhdl
072400010202     C                   movel     'M'           parmbr
072500070530     C                   eval      parccm = %subst(vlrKSC:2:7)
072600010202     C                   eval      paropz = '1'
072700100525     C*
072800010202     C* Effettuo la chiamata al CLLE preposto
072900100525     C  N51              call(e)   'TITVVTC'
073000010202     C                   parm                    parccm
073100010202     C                   parm                    parmbr
073200010202     C                   parm                    paropz
073300100525     C   51              call(e)   'TITVVBC'
073400100525     C                   parm                    parccm
073500100525     C                   parm                    parmbr
073600100525     C                   parm                    paropz
073700100525     C*
073800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
073900010202     C                   if        %error
074000010202     C                   movel     '1'           chkcall
074100010202     C                   else
074200010202     C                   movel     '0'           chkcall
074300010202     C                   endif
074400010202     C*
074500010202     C                   ENDSR
074600000801     C*----------------------------------------------------*
074700000801     C*  CONTROLLO NUMERICITA' CAMPI
074800000801     C*----------------------------------------------------*
074900000801     C     CHKNUM        BEGSR
075000081113     C*
075100081113     C                   IF        PiDecChr = *blanks
075200110201     C                   EVAL      PiDecChr = ','
075300081113     C                   ENDIF
075400091223     C*
075500081113     C                   callp(e)  UBISNUM_Check(PiStr
075600081113     C                                          :PiDecChr
075700081113     C                                          :PiVal
075800081113     C                                          :PiNum
075900081113     C                                          :PiInt)
076000081113     C*
076100000801     C                   IF        %error
076200000801     C                   EVAL      PiInt=*off
076300000801     C                   ENDIF
076400000801     C*
076500000801     C                   ENDSR
076600000801     C***
076700011113
076800011113
076900000801
077000000801
077100990920      /TITLE Invio dei dati al punto operativo.
077200010202     C     invio         BEGSR
077300990920     C*
077400021113     C* 1° invio FIVAT
077500010201     C                   reset                   dscmz
077600010201     C                   move      vlrpoi        cmzdst
077700021113     C                   eval      cmzfld = 'FIVATWWR'
077800010201     C                   eval      cmzmbd = vlrhdl
077900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
078000021009     C***                if        prmfir = *blanks
078100021113     C                   eval      cmzfla = 'FIVAT00F'
078200021113     C                   eval      cmzmba = 'FIVAT00F'
078300021009     C***                else
078400021009     C***                eval      cmzfla = prmfir
078500021009     C***                eval      cmzmba = prmfir
078600021009     C***                endif
078700010201     C                   eval      cmznrr = *zeros
078800020305     C                   move      §ctrokvt      cmznrr
078900021018     C                   eval      cmzlba = vlrfl1
079000010201     C                   call(e)   'TIS711C'
079100010201     C                   parm                    dscmz
079200010201     C                   parm      *blanks       esito
079300010205     C                   if        %error
079400010205     C                             or cmzerr = '1'
079500010205     C                             or esito  = '1'
079600010205     C                   eval      wrkesito = '3'
079700010205     C                   else
079800010201     C*
079900021113     C* 2° invio FIVAB
080000100525     C                   if        *in51 = *off
080100010201     C                   reset                   dscmz
080200010201     C                   move      vlrpoi        cmzdst
080300010201     C                   eval      cmzfld = vlrfou
080400010201     C                   eval      cmzmbd = vlrhdl
080500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
080600021009     C***                if        prmfir = *blanks
080700021113     C                   eval      cmzfla = 'FIVAB00F'
080800021113     C                   eval      cmzmba = 'FIVAB00F'
080900021009     C***                else
081000021009     C***                eval      cmzfla = prmfir
081100021009     C***                eval      cmzmba = prmfir
081200021009     C***                endif
081300010201     C                   eval      cmznrr = *zeros
081400010201     C                   move      §ctrokvb      cmznrr
081500021018     C                   eval      cmzlba = vlrfl1
081600010201     C                   call(e)   'TIS711C'
081700010201     C                   parm                    dscmz
081800010201     C                   parm      *blanks       esito
081900010201     C                   if        %error
082000010201     C                             or cmzerr = '1'
082100010201     C                             or esito  = '1'
082200010201     C                   eval      wrkesito = '3'
082300010201     C                   endif
082400100525     C                   endif
082500010205     C                   endif
082600990920     C*
082700000613     C                   ENDSR
082800000613     C***
082900070411
083000070411     C     *pssr         BEGSR
083100070411     C*
083200070411     C                   if        %open(tivin00r)
083300070411     C                   close     tivin00r
083400070411     C                   endif
083500070411     C                   if        %open(fivabwwr)
083600070411     C                   close     fivabwwr
083700070411     C                   endif
083800070411     C                   if        %open(fivatwwr)
083900070411     C                   close     fivatwwr
084000070411     C                   endif
084100070411     C*
084200070411     C* Effettuo la chiamata al CLLE preposto
084300100525     C  N51              call(e)   'TITVVTC'
084400100525     C                   parm                    parccm
084500100525     C                   parm                    parmbr
084600100525     C                   parm      '2'           paropz
084700100525     C   51              call(e)   'TITVVBC'
084800100525     C                   parm                    parccm
084900100525     C                   parm                    parmbr
085000100525     C                   parm      '2'           paropz
085100070411     C*
085200070411     C                   eval      wrkesito = '2'
085300070411     C*
085400070411     C                   seton                                        LR
085500070411     C*
085600070411     C                   ENDSR     '*CANCL'
085700070411     C***
085800070411
085900990910
086000000613     C     *inzsr        BEGSR
086100990910     C*
086200990910     C     *entry        plist
086300990920     C                   parm                    tivlrds
086400990921     C                   parm      wrkesito      esito
086500000724     C                   parm                    prmlit
086600000710     C                   parm                    prmfir
086700000613     C*
086800000830     C* CALCOLA LA DATA CORRENTE
086900091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
087000091223     C                   eval      datcor = %dec(%date() : *ISO)
087100120420     C*
087200120420     C* Chiave su TABEL00F - parziale
087300120420     C     KEYtabP       KLIST
087400120420     C                   KFLD                    tblKUT
087500120420     C                   KFLD                    tblCOD
087600000830     C*
087700000613     C                   ENDSR
087800000613     C***
