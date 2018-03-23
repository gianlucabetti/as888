000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600100412     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800100928     Ftitv2a3p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100100928     Ftitv2a3ps O    f  198        PRINTER usropn
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
003600100928     D depspe          s              5    INZ(*blanks)
003700100928     D curspe          s              5    INZ(*blanks)
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
011600100928     C                   if        not %open(titv2a3ps)
011700100928     C                   open      titv2a3ps
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
014100100928     C                   if        %open(titv2a3ps)
014200000616     C                   except    findett
014300100928     C                   close     titv2a3ps
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
016200100928     C                   if        not %open(titv2a3p)
016300100928     C                   open      titv2a3p
016400990915     C                   endif
016500990915     C*
016600990915     C                   except    riepilogo
016700990915     C*
016800100928     C                   if        %open(titv2a3p)
016900100928     C                   close     titv2a3p
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
020800100928     C+ vindta, vinflg, substr(vindta, 6, 5) as sped, rrn(tivin00r)
020900060223     C+ from tivin00r
021000100928     C+ order by sped, substr(vindta, 208, 6) desc
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
023500100928     C                   EVAL      curspe=%trim(%subst(vindta:6:5))
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
038900100928     C                   EVAL      VABCCM = 0360472
039000100928     C                   EVAL      VATCCM = 0360472
039100100928     C                   EVAL      VABLNP = 036
039200100928     C                   EVAL      VATLNP = 036
039300100928     C                   EVAL      VABCTR = 000
039400100928     C                   EVAL      VABCTM = '7Q'
039500100928     C                   EVAL      VABTSP = 'C'
039600100525     C                   EVAL      VABCBO = '1'
039700070222     C* ... e poi verifico se sono stati passati come parametri
039800070222     C                   IF        vlrppt > *blanks
039900100525     C*
040000070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
040100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
040200070222     C                   EXSR      CHKNUM
040300070222     C                   IF        PiInt=*on
040400070222     C                   Z-ADD     PiVal         VABCCM
040500070222     C                   Z-ADD     PiVal         VATCCM
040600070222     C                   ENDIF
040700070222     C                   ENDIF
040800070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
040900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
041000070222     C                   EXSR      CHKNUM
041100070222     C                   IF        PiInt=*on
041200070222     C                   Z-ADD     PiVal         VABLNP
041300070222     C                   Z-ADD     PiVal         VATLNP
041400070222     C                   ENDIF
041500070222     C                   ENDIF
041600070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
041700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
041800070222     C                   EXSR      CHKNUM
041900070222     C                   IF        PiInt=*on
042000070222     C                   Z-ADD     PiVal         VABCTR
042100070222     C                   ENDIF
042200070928     C                   ENDIF
042300071009     C                   MOVEL     *blanks       wDISK             1
042400071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
042500071009     C                   IF        wDISK <> *blanks
042600070928     C                   SETON                                        50
042700070222     C                   ENDIF
042800100525     C                   IF        wDISK  = 'T'
042900100525     C                   SETOFF                                       50
043000100525     C                   SETON                                        51
043100100525     C                   ENDIF
043200100401     C                   MOVEL     *blanks       wDUPREC           1
043300100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
043400100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
043500100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
043600100525     C                   ENDIF
043700100525     C*
043800070222     C                   ENDIF
043900071009     C*
044000071009     C   50              EVAL      VABCTM = '7Q'
044100000801     C*
044200000801     C                   ENDSR
044300091223     C*----------------------------------------------------*
044400091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
044500091223     C*----------------------------------------------------*
044600091223     C     REPNSP        BEGSR
044700091223     C*
044800091223     C                   EXSR      INZVAR
044900091223     C                   EXSR      DEFCAM
045000100928     C*
045100100928     C                   SETON                                        60
045200091223     C*
045300091223     C* NSP => Stacco un numeratore da AZNUM
045400091223     C                   clear                   TRUL33DS
045500091223     C                   eval      I33OPE = *zeros
045600091223     C                   eval      I33CNU = 302
045700091223     C                   eval      I33NUM = 1
045800091223     C                   movel     TRUL33DS      KPJBU
045900091223     C                   call      'TRUL33R'
046000091223     C                   parm                    KPJBA
046100091223     C                   movel     KPJBU         TRUL33DS
046200091223     C                   if        O33ERR = *zeros
046300091223     C                   z-add     O33NRF        VABNSP
046400091223     C                   z-add     O33NRF        VATNSP
046500091223     C                   else
046600091223     C                   SETON                                        31
046700091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046800091223     C                             + ' ' + 'VABNSP VATNSP'
046900091223     C                   endif
047000091223     C*
047100091223     C                   ENDSR
047200000801     C*----------------------------------------------------*
047300021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
047400000801     C*----------------------------------------------------*
047500100730     C     IMPVAB        BEGSR
047600070530     C*
047700070530     C                   SETOFF                                       3132
047800070928     C*
047900070928     C                   MOVE(P)   vlrpoi        VABFGS
048000070928     C                   MOVE(P)   vlrpoi        VATFGS
048100070928     C*
048200070928     C                   MOVEL     datcor        VABAAS
048300070928     C                   MOVEL     datcor        VATAAS
048400070928     C                   MOVE      datcor        VABMGS
048500100729     C*
048600100729     C* Reperimento campi ALFA
048700100928     C                   EVAL      VABRSD=%trim(%subst(vindta:46:35))
048800100928     C                   EVAL      VABRD2=%trim(%subst(vindta:81:35))
048900100928     C                   EVAL      VABIND=%trim(%subst(vindta:116:35))
049000100928     C                   EVAL      VABLOD=%trim(%subst(vindta:151:35))
049100100928     C                   EVAL      VABPRD=%trim(%subst(vindta:186:2))
049200101118     C***                EVAL      VABRMA=%trim(%subst(vindta:11:6))
049300101118     C                   EVAL      VABRMA=%trim(%subst(vindta:216:3))
049400100928     C                   EVAL      VABNOT=%trim(%subst(vindta:258:35))
049500101028     C*
049600101028     C                   IF        %subst(vindta:258:10)='TELEFONARE'
049700101028     C                   EVAL      VABTC1 = 'A'
049800101028     C                   ENDIF
049900100729     C*
050000100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
050100100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
050200100729     C     '@':'A'       XLATE     VABRSD        VABRSD
050300100729     C* ==
050400100729     C*
050500100928     C* CCM
050600100730     C                   SELECT
050700100928     C                   WHEN      %subst(vindta:294:7) = '0360472'
050800100928     C                   EVAL      VABCBO = '1'
050900100928     C                   WHEN      %subst(vindta:294:7) = '0360705'
051000100928     C                   EVAL      VABCCM = 0360705
051100100928     C                   EVAL      VATCCM = 0360705
051200100928     C                   EVAL      VABCBO = '4'
051300100928     C                   EVAL      VABTIC = 'BM'
051400100928     C                   WHEN      %subst(vindta:294:7) = '0360706'
051500100928     C                   EVAL      VABCCM = 0360706
051600100928     C                   EVAL      VATCCM = 0360706
051700100928     C                   EVAL      VABCBO = '4'
051800100928     C                   EVAL      VABTIC = 'TM'
051900100730     C                   ENDSL
052000100729     C*
052100100729     C* Reperimento campi NUMERICI
052200100729     C                   MOVEL     DATCOR        VABAAS
052300100729     C                   MOVE      DATCOR        VABMGS
052400100730     C* RMN
052500100928     C                   EVAL      PiStr=%trim(%subst(vindta:6:5))
052600100730     C                   EXSR      CHKNUM
052700100730     C                   IF        PiInt=*on
052800100730     C                   Z-ADD     PiVal         VABRMN
052900100928     C  N60              Z-ADD     PiVal         VABNSP
053000100928     C  N60              Z-ADD     PiVal         VATNSP
053100100730     C                   ELSE
053200100730     C                   SETON                                        32
053300100730     C                   Z-ADD     1             VABRMN
053400100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053500100928     C                             + ' ' + 'VABNSP VATNSP VABRMN'
053600100730     C                   ENDIF
053700100729     C* DCR
053800100730     C                   IF        %subst(vindta:443:6) <> *blanks AND
053900100730     C                             %subst(vindta:443:6) <> *zeros
054000100928     C                   EVAL      PiStr=%trim(%subst(vindta:237:8))
054100100729     C                   EXSR      CHKNUM
054200100729     C                   IF        PiInt=*on
054300100729     C                   Z-ADD     PiVal         VABDCR
054400100729     C                   ELSE
054500100729     C                   SETON                                        32
054600100729     C                   Z-ADD     *zeros        VABDCR
054700100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054800100729     C                             + ' ' + 'VABDCR'
054900100729     C                   ENDIF
055000100729     C                   ENDIF
055100100729     C* NCL
055200100928     C                   IF        %subst(vindta:304:4) <> *blanks AND
055300100928     C                             %subst(vindta:304:4) <> *zeros
055400100928     C                   EVAL      PiStr=%trim(%subst(vindta:304:4))
055500100928     C                   EXSR      CHKNUM
055600100928     C                   IF        PiInt=*on
055700100928     C                   Z-ADD     PiVal         VABNCL
055800100928     C                   ELSE
055900100928     C                   SETON                                        32
056000100928     C                   Z-ADD     *zeros        VABNCL
056100100928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056200100928     C                             + ' ' + 'VABNCL'
056300100928     C                   ENDIF
056400100928     C                   ELSE
056500100928     C                   EVAL      PiStr=%trim(%subst(vindta:188:4))
056600100729     C                   EXSR      CHKNUM
056700100729     C                   IF        PiInt=*on
056800100729     C                   Z-ADD     PiVal         VABNCL
056900100729     C                   ELSE
057000100729     C                   SETON                                        32
057100100729     C                   Z-ADD     *zeros        VABNCL
057200100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
057300100729     C                             + ' ' + 'VABNCL'
057400100928     C                   ENDIF
057500100729     C                   ENDIF
057600100729     C* PKB
057700100928     C                   EVAL      PiStr=%trim(%subst(vindta:208:6))
057800100729     C                   EXSR      CHKNUM
057900100729     C                   IF        PiNum=*on
058000100928     C                   EVAL      PiVal=PiVal/10                               * gestisco 1 dec.
058100100729     C                   Z-ADD(H)  PiVal         VABPKB
058200100729     C                   ELSE
058300100729     C                   SETON                                        32
058400100729     C                   Z-ADD     *zeros        VABPKB
058500100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
058600100729     C                             + ' ' + 'VABPKB'
058700100729     C                   ENDIF
058800100729     C* CAS
058900100928     C                   IF        %subst(vindta:250:8)<> *blanks AND
059000100928     C                             %subst(vindta:250:8)<> *zeros
059100100729     C                   EVAL      FlgCAS = '1'
059200100729     C                   EVAL      VABVCA = 'EUR'
059300100928     C                   EVAL      PiStr=%trim(%subst(vindta:250:8))
059400100729     C                   EXSR      CHKNUM
059500100729     C                   IF        PiNum=*on
059600100928     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
059700100928     C                   Z-ADD(H)  PiVal         VABCAS
059800100729     C                   ELSE
059900100729     C                   SETON                                        32
060000100729     C                   Z-ADD     *zeros        VABCAS
060100100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
060200100729     C                             + ' ' + 'VABCAS'
060300100729     C                   ENDIF
060400100729     C                   ENDIF
060500100729     C*
060600100729     C* Se provincia nn valorizzata la reperisco
060700100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
060800100729     C                   IF        VABNZD  = *blanks AND
060900100729     C                             VABPRD  = *blanks AND
061000100729     C                             VABCAD <> *blanks
061100100729     C                   CLEAR                   TISI95DS
061200100729     C                   EVAL      I95TCN = '3'
061300100729     C                   Z-ADD     datcor        I95DAT
061400100729     C                   EVAL      I95NAR = VABNZD
061500100729     C                   EVAL      I95CAP = VABCAD
061600100729     C                   EVAL      I95LOC = VABLOD
061700100729     C                   CALL      'TISI95R'
061800100729     C                   PARM                    TISI95DS
061900100729     C                   EVAL      VABPRD = O95PRV
062000100729     C                   ENDIF
062100100729     C*
062200100729     C* Considerazioni finali su CBO/CAS
062300100729     C                   IF        FlgCAS = '1'
062400100729     C                   IF        VABCBO = '1'
062500100729     C                   EVAL      VABCBO = '4'
062600100729     C                   ENDIF
062700100729     C                   IF        VABCBO = '2'
062800100729     C                   EVAL      VABCBO = '6'
062900100729     C                   ENDIF
063000100729     C                   ENDIF
063100010202     C*
063200000801     C* Ebbene...
063300000801     C                   ADD       1             §CTRMO
063400070530     C                   IF        *in31 <> *zeros OR
063500070530     C                             *in32 <> *zeros
063600000801     C                   ADD       1             §CTRNO
063700000801     C                   EVAL      recko = vindta
063800000801     C                   EXCEPT    rigadett
063900020725     C                   EVAL      x_vinflg = '2'
064000000801     C                   ELSE
064100010201     C                   ADD       1             §CTROKVB
064200000801     C                   ENDIF
064300000801     C*
064400000801     C                   ENDSR
064500100127     C*----------------------------------------------------*
064600100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
064700100127     C*----------------------------------------------------*
064800100127     C     WRIVAT_A      BEGSR
064900100127     C*
065000100127     C* Valorizzo l buffer di scrittura del FIVAT
065100100127     C* - TIPO RECORD "A"
065200100412     C***                eval      VATTRC = 'A'
065300100412     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
065400100412     C***                exsr      wrivat
065500100127     C*
065600100127     C                   ENDSR
065700010201     C*----------------------------------------------------*
065800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
065900010201     C*----------------------------------------------------*
066000071003     C     WRIVAT_B      BEGSR
066100010201     C*
066200021113     C* Valorizzo l buffer di scrittura del FIVAT
066300070928     C* - TIPO RECORD "B"
066400100412     C***                eval      VATTRC = 'B'
066500100412     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
066600100412     C***                exsr      wrivat
066700010201     C*
066800010201     C                   ENDSR
066900071003     C*----------------------------------------------------*
067000071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
067100071003     C*----------------------------------------------------*
067200071003     C     WRIVAT_E      BEGSR
067300071003     C*
067400071003     C* Valorizzo l buffer di scrittura del FIVAT
067500071003     C* - TIPO RECORD "E"
067600100729     C*
067700100928     C                   eval      VATTRC = 'E'
067800100928     C                   eval      VATNOT = %trim(%subst(vindta:192:16))
067900100928     C                   exsr      wrivat
068000071003     C*
068100071003     C                   ENDSR
068200100127     C*----------------------------------------------------*
068300100127     C*  SCARICO BUFFER FIVAT
068400100127     C*----------------------------------------------------*
068500100127     C     WRIVAT        BEGSR
068600100127     C*
068700100127     C                   if        vatNOT <> *blanks
068800100127     C                   ADD       1             §CTROKVT
068900100127     C                   write     FIVAT000
069000100127     C                   endif
069100100127     C*
069200100127     C                   ENDSR
069300020904
069400020904
069500010202     C*----------------------------------------------------*
069600100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
069700010202     C*----------------------------------------------------*
069800100525     C     PREVASX       BEGSR
069900100525     C*
070000021113     C* Compongo il nome del membro da dare al FIVATWWR
070100010202     C                   eval      parmbr = vlrhdl
070200010202     C                   movel     'M'           parmbr
070300070530     C                   eval      parccm = %subst(vlrKSC:2:7)
070400010202     C                   eval      paropz = '1'
070500100525     C*
070600010202     C* Effettuo la chiamata al CLLE preposto
070700100525     C  N51              call(e)   'TITVVTC'
070800010202     C                   parm                    parccm
070900010202     C                   parm                    parmbr
071000010202     C                   parm                    paropz
071100100525     C   51              call(e)   'TITVVBC'
071200100525     C                   parm                    parccm
071300100525     C                   parm                    parmbr
071400100525     C                   parm                    paropz
071500100525     C*
071600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
071700010202     C                   if        %error
071800010202     C                   movel     '1'           chkcall
071900010202     C                   else
072000010202     C                   movel     '0'           chkcall
072100010202     C                   endif
072200010202     C*
072300010202     C                   ENDSR
072400000801     C*----------------------------------------------------*
072500000801     C*  CONTROLLO NUMERICITA' CAMPI
072600000801     C*----------------------------------------------------*
072700000801     C     CHKNUM        BEGSR
072800081113     C*
072900081113     C                   IF        PiDecChr = *blanks
073000100503     C                   EVAL      PiDecChr = '.'
073100081113     C                   ENDIF
073200091223     C*
073300081113     C                   callp(e)  UBISNUM_Check(PiStr
073400081113     C                                          :PiDecChr
073500081113     C                                          :PiVal
073600081113     C                                          :PiNum
073700081113     C                                          :PiInt)
073800081113     C*
073900000801     C                   IF        %error
074000000801     C                   EVAL      PiInt=*off
074100000801     C                   ENDIF
074200000801     C*
074300000801     C                   ENDSR
074400000801     C***
074500011113
074600011113
074700000801
074800000801
074900990920      /TITLE Invio dei dati al punto operativo.
075000010202     C     invio         BEGSR
075100990920     C*
075200021113     C* 1° invio FIVAT
075300010201     C                   reset                   dscmz
075400010201     C                   move      vlrpoi        cmzdst
075500021113     C                   eval      cmzfld = 'FIVATWWR'
075600010201     C                   eval      cmzmbd = vlrhdl
075700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075800021009     C***                if        prmfir = *blanks
075900021113     C                   eval      cmzfla = 'FIVAT00F'
076000021113     C                   eval      cmzmba = 'FIVAT00F'
076100021009     C***                else
076200021009     C***                eval      cmzfla = prmfir
076300021009     C***                eval      cmzmba = prmfir
076400021009     C***                endif
076500010201     C                   eval      cmznrr = *zeros
076600020305     C                   move      §ctrokvt      cmznrr
076700021018     C                   eval      cmzlba = vlrfl1
076800010201     C                   call(e)   'TIS711C'
076900010201     C                   parm                    dscmz
077000010201     C                   parm      *blanks       esito
077100010205     C                   if        %error
077200010205     C                             or cmzerr = '1'
077300010205     C                             or esito  = '1'
077400010205     C                   eval      wrkesito = '3'
077500010205     C                   else
077600010201     C*
077700021113     C* 2° invio FIVAB
077800100525     C                   if        *in51 = *off
077900010201     C                   reset                   dscmz
078000010201     C                   move      vlrpoi        cmzdst
078100010201     C                   eval      cmzfld = vlrfou
078200010201     C                   eval      cmzmbd = vlrhdl
078300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
078400021009     C***                if        prmfir = *blanks
078500021113     C                   eval      cmzfla = 'FIVAB00F'
078600021113     C                   eval      cmzmba = 'FIVAB00F'
078700021009     C***                else
078800021009     C***                eval      cmzfla = prmfir
078900021009     C***                eval      cmzmba = prmfir
079000021009     C***                endif
079100010201     C                   eval      cmznrr = *zeros
079200010201     C                   move      §ctrokvb      cmznrr
079300021018     C                   eval      cmzlba = vlrfl1
079400010201     C                   call(e)   'TIS711C'
079500010201     C                   parm                    dscmz
079600010201     C                   parm      *blanks       esito
079700010201     C                   if        %error
079800010201     C                             or cmzerr = '1'
079900010201     C                             or esito  = '1'
080000010201     C                   eval      wrkesito = '3'
080100010201     C                   endif
080200100525     C                   endif
080300010205     C                   endif
080400990920     C*
080500000613     C                   ENDSR
080600000613     C***
080700070411
080800070411     C     *pssr         BEGSR
080900070411     C*
081000070411     C                   if        %open(tivin00r)
081100070411     C                   close     tivin00r
081200070411     C                   endif
081300070411     C                   if        %open(fivabwwr)
081400070411     C                   close     fivabwwr
081500070411     C                   endif
081600070411     C                   if        %open(fivatwwr)
081700070411     C                   close     fivatwwr
081800070411     C                   endif
081900070411     C*
082000070411     C* Effettuo la chiamata al CLLE preposto
082100100525     C  N51              call(e)   'TITVVTC'
082200100525     C                   parm                    parccm
082300100525     C                   parm                    parmbr
082400100525     C                   parm      '2'           paropz
082500100525     C   51              call(e)   'TITVVBC'
082600100525     C                   parm                    parccm
082700100525     C                   parm                    parmbr
082800100525     C                   parm      '2'           paropz
082900070411     C*
083000070411     C                   eval      wrkesito = '2'
083100070411     C*
083200070411     C                   seton                                        LR
083300070411     C*
083400070411     C                   ENDSR     '*CANCL'
083500070411     C***
083600070411
083700990910
083800000613     C     *inzsr        BEGSR
083900990910     C*
084000990910     C     *entry        plist
084100990920     C                   parm                    tivlrds
084200990921     C                   parm      wrkesito      esito
084300000724     C                   parm                    prmlit
084400000710     C                   parm                    prmfir
084500000613     C*
084600000830     C* CALCOLA LA DATA CORRENTE
084700091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
084800091223     C                   eval      datcor = %dec(%date() : *ISO)
084900000830     C*
085000000613     C                   ENDSR
085100000613     C***
085200990908
085300100928     Otitv2a3p  E            riepilogo         2
085400990915     O                                              'Upload via Internet'
085500990915     O                                           +1 'Traduzione TIVIN00R -'
085600021113     O                                           +1 'FIVABWWR/FIVATWWR'
085700010201     O                                           +1 'Report testate bolle'
085800990915     O          E            riepilogo   2
085900990915     O                       wrkdata
086000990915     O                       wrkora              +1
086100990915     O                       procname            +1
086200990915     O          E            riepilogo   2
086300990915     O                                              'Cliente..................:'
086400990915     O                       VABCCM        z     +1
086500990915     O          E            riepilogo   2
086600990920     O                                              'Riferimento Strategi.....:'
086700990920     O                       vlrhdl              +1
086800990915     O          E            riepilogo   2
086900990915     O                                              'Giusti...................:'
087000010201     O                       §CTROKVB      2   +  1
087100100525     O                       §CTROKVT      2   +  1
087200990915     O          E            riepilogo   2
087300990915     O                                              'Sbagliati e corretti.....:'
087400971022     O                       §CTRMO        2   +  1
087500990915     O          E            riepilogo   2
087600990915     O                                              'Sbagliati e scartati.....:'
087700971022     O                       §CTRNO        2   +  1
087800000613
087900100928     Otitv2a3ps E            testdett          2
088000000613     O                                              'Upload via Internet'
088100000613     O                                           +1 'Traduzione TIVIN00R -'
088200021113     O                                           +1 'FIVABWWR/FIVATWWR'
088300010201     O                                           +1 'Report testate bolle'
088400000616     O          E            testdett    3
088500000613     O                                           +2 'N° rec'
088600000613     O                                           +3 'Anteprima contenuto'
088700000616     O          E            rigadett    2
088800000613     O                       rrnum               +2
088900000621     O                       recko               +3
089000000616     O          E            findett     2
089100000613     O                       wrkdata
089200000613     O                       wrkora              +1
089300000613     O                       procname            +1
089400000616     O          E            findett     2
089500000613     O                                              'Cliente..................:'
089600000613     O                       VABCCM        z     +1
089700000616     O          E            findett     2
089800000613     O                                              'Riferimento Strategi.....:'
089900000613     O                       vlrhdl              +1
090000000616     O          E            findett     2
090100000613     O                                              'Giusti...................:'
090200010201     O                       §CTROKVB      2   +  1
090300100525     O                       §CTROKVT      2   +  1
090400000616     O          E            findett     2
090500000613     O                                              'Sbagliati e corretti.....:'
090600000613     O                       §CTRMO        2   +  1
090700000616     O          E            findett     2
090800000613     O                                              'Sbagliati e scartati.....:'
090900000613     O                       §CTRNO        2   +  1
091000000512** CMD - COMANDI CL
091100100928OVRPRTF FILE(TITV2A3P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
091200100928OVRPRTF FILE(TITV2A3PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
091300100928DLTOVR FILE(TITV2A3P TITV2A3PS) LVL(*)
091400000512
091500000512
