000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600100412     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800100629     Ftitv1y4p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100100629     Ftitv1y4ps O    f  198        PRINTER usropn
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
003600100629     D depspe          s              7    INZ(*blanks)
003700100629     D curspe          s              7    INZ(*blanks)
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
004900020725     D  VINDTA                 1   2048
005000020725     D  VINFLG              2049   2049
005100020725     D  VINSPE              2050   2059
005200020725     D  VINRRN              2060   2067  0
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
011600100629     C                   if        not %open(titv1y4ps)
011700100629     C                   open      titv1y4ps
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
014100100629     C                   if        %open(titv1y4ps)
014200000616     C                   except    findett
014300100629     C                   close     titv1y4ps
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
016200100629     C                   if        not %open(titv1y4p)
016300100629     C                   open      titv1y4p
016400990915     C                   endif
016500990915     C*
016600990915     C                   except    riepilogo
016700990915     C*
016800100629     C                   if        %open(titv1y4p)
016900100629     C                   close     titv1y4p
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
020800100629     C+ vindta, vinflg, substr(vindta, 10, 7) as sped, rrn(tivin00r)
020900060223     C+ from tivin00r
021000060223     C+ order by sped
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
023500100629     C                   EVAL      curspe=%trim(%subst(vindta:10:7))
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
027600100412     C                   if        wDUPREC <> 'N'
027700020305     C                   exsr      impvab
027800100127     C                   exsr      wrivat_a                                     => carico VAT
027900071003     C                   exsr      wrivat_b                                     => carico VAT
028000071003     C   50              exsr      wrivat_e                                     => carico VAT
028100100401     C                   endif
028200010305     C                   endif
028300010305     C                   endif
028400010305     C                   endif
028500071003     C                   endif
028600071009     C                   endif
028700000905     C*
028800000905     C                   else
028900020725     C                   eval      x_vinflg = '1'
029000000905     C                   endif
029100000905     C*
029200020725     C     VINRRN        chain     tivin000
029300020725     C                   if        %found(tivin00r)
029400020725     C                   eval      y_vinflg = x_vinflg
029500020725     C                   eval      y_vinmsg = x_vinmsg
029600020725     C                   update    tivin000
029700020725     C                   endif
029800020725     C*
029900020725     C/EXEC SQL
030000070530     C+ Fetch C1 into :INPUT_DS
030100020725     C/END-EXEC
030200020725     C*
030300020725     C                   enddo
030400020725     C*
030500020725     C/EXEC SQL
030600020725     C+ close C1
030700020725     C/END-EXEC
030800000905     C*
030900020305     C* Scarico i VAB rimasti "in sospeso"
031000071009     C                   if        wDISK = 'C'
031100100412     C                   exsr      wrivab
031200071009     C                   endif
031300010202     C*
031400010202     C                   endif
031500990910
031600990910     C* Se non ci sono record con errori ...
031700000710     C                   if        §ctrno = 0
031800990910     C* ... restituisco esito OK.
031900990921     C                   eval      wrkesito = '0'
032000990910     C                   else
032100100525     C                   if        §ctrokvb > 0 OR
032200100525     C                             §ctrokvt > 0
032300990921     C                   eval      wrkesito = '1'
032400000710     C                   else
032500000710     C                   eval      wrkesito = '2'
032600990910     C                   endif
032700000710     C                   endif
032800990910     C*
032900990914     C                   if        %open(tivin00r)
033000990908     C                   close     tivin00r
033100990914     C                   endif
033200021113     C                   if        %open(fivabwwr)
033300021113     C                   close     fivabwwr
033400990914     C                   endif
033500021113     C                   if        %open(fivatwwr)
033600021113     C                   close     fivatwwr
033700010201     C                   endif
033800990910     C*
033900100525     C                   if        §ctrokvb > 0 OR
034000100525     C                             §ctrokvt > 0
034100000724     C                             and vlrpoi <> *zeros
034200010202     C                   exsr      invio
034300990920     C                   endif
034400990920     C*
034500910830     C                   ENDSR
034600000613     C***
034700100412
034800100412
034900010305
035000010305     C*----------------------------------------------------*
035100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
035200010305     C*----------------------------------------------------*
035300020305     C     WRIVAB        BEGSR
035400100412     C*
035500100412     C* Gestisco l'eventuale rottura x numero spedizione
035600100412     C                   if        VABRMA = *blanks
035700100412     C                   movel(P)  VABRMN        VABRMA
035800100412     C                   endif
035900100412     C*
036000100525     C                   if        *in51 = *off and *in31 = *off
036100100525     C                   write     fivab000                                     => scarico il VAB
036200100525     C                   endif
036300010305     C*
036400010305     C                   ENDSR
036500990920
036600000801     C*----------------------------------------------------*
036700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
036800000801     C*----------------------------------------------------*
036900010201     C     INZVAR        BEGSR
037000000801     C*
037100010201     C                   Z-ADD     *zeros        Num5_0
037200020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
037300020725     C                   MOVEL     '0'           FlgCAS            1
037400000801     C*
037500000801     C                   ENDSR
037600000801     C*----------------------------------------------------*
037700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
037800000801     C*----------------------------------------------------*
037900020904     C     DEFCAM        BEGSR
038000080923     C*
038100100525     C                   SETOFF                                       5051
038200000801     C*
038300021113     C                   CLEAR                   FIVAB000
038400021113     C                   CLEAR                   FIVAT000
038500070730     C* Imposto i valori di default...
038600100629     C                   EVAL      VABCCM = 2494470
038700100629     C                   EVAL      VATCCM = 2494470
038800100629     C                   EVAL      VABLNP = 249
038900100629     C                   EVAL      VATLNP = 249
039000100525     C                   EVAL      VABCTR = 000
039100100525     C                   EVAL      VABCBO = '1'
039200070222     C* ... e poi verifico se sono stati passati come parametri
039300070222     C                   IF        vlrppt > *blanks
039400100525     C*
039500070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
039600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
039700070222     C                   EXSR      CHKNUM
039800070222     C                   IF        PiInt=*on
039900070222     C                   Z-ADD     PiVal         VABCCM
040000070222     C                   Z-ADD     PiVal         VATCCM
040100070222     C                   ENDIF
040200070222     C                   ENDIF
040300070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
040400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
040500070222     C                   EXSR      CHKNUM
040600070222     C                   IF        PiInt=*on
040700070222     C                   Z-ADD     PiVal         VABLNP
040800070222     C                   Z-ADD     PiVal         VATLNP
040900070222     C                   ENDIF
041000070222     C                   ENDIF
041100070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
041200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
041300070222     C                   EXSR      CHKNUM
041400070222     C                   IF        PiInt=*on
041500070222     C                   Z-ADD     PiVal         VABCTR
041600070222     C                   ENDIF
041700070928     C                   ENDIF
041800071009     C                   MOVEL     *blanks       wDISK             1
041900071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
042000071009     C                   IF        wDISK <> *blanks
042100070928     C                   SETON                                        50
042200070222     C                   ENDIF
042300100525     C                   IF        wDISK  = 'T'
042400100525     C                   SETOFF                                       50
042500100525     C                   SETON                                        51
042600100525     C                   ENDIF
042700100401     C                   MOVEL     *blanks       wDUPREC           1
042800100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
042900100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
043000100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
043100100525     C                   ENDIF
043200100525     C*
043300070222     C                   ENDIF
043400071009     C*
043500071009     C   50              EVAL      VABCTM = '7Q'
043600000801     C*
043700000801     C                   ENDSR
043800091223     C*----------------------------------------------------*
043900091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
044000091223     C*----------------------------------------------------*
044100091223     C     REPNSP        BEGSR
044200091223     C*
044300091223     C                   EXSR      INZVAR
044400091223     C                   EXSR      DEFCAM
044500091223     C*
044600091223     C* NSP => Stacco un numeratore da AZNUM
044700091223     C                   clear                   TRUL33DS
044800091223     C                   eval      I33OPE = *zeros
044900091223     C                   eval      I33CNU = 302
045000091223     C                   eval      I33NUM = 1
045100091223     C                   movel     TRUL33DS      KPJBU
045200091223     C                   call      'TRUL33R'
045300091223     C                   parm                    KPJBA
045400091223     C                   movel     KPJBU         TRUL33DS
045500091223     C                   if        O33ERR = *zeros
045600091223     C                   z-add     O33NRF        VABNSP
045700091223     C                   z-add     O33NRF        VATNSP
045800091223     C                   else
045900091223     C                   SETON                                        31
046000091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046100091223     C                             + ' ' + 'VABNSP VATNSP'
046200091223     C                   endif
046300091223     C*
046400091223     C                   ENDSR
046500000801     C*----------------------------------------------------*
046600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
046700000801     C*----------------------------------------------------*
046800010201     C     IMPVAB        BEGSR
046900070530     C*
047000070530     C                   SETOFF                                       3132
047100070928     C*
047200070928     C                   MOVE(P)   vlrpoi        VABFGS
047300070928     C                   MOVE(P)   vlrpoi        VATFGS
047400070928     C*
047500070928     C                   MOVEL     datcor        VABAAS
047600070928     C                   MOVEL     datcor        VATAAS
047700070928     C                   MOVE      datcor        VABMGS
047800100629     C                   EVAL      VABRSD=%trim(%subst(vindta:17:30))
047900100503     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
048000100503     C     '@':'A'       XLATE     VABRSD        VABRSD
048100100503     C* ==
048200100629     C                   EVAL      VABIND=%trim(%subst(vindta:47:30))
048300100629     C                   EVAL      VABLOD=%trim(%subst(vindta:77:28))
048400100629     C                   EVAL      VABPRD=%trim(%subst(vindta:112:2))
048500100629     C                   EVAL      VABRMA=%trim(%subst(vindta:10:7))
048600100520     C*
048700100525     C* NSP
048800100525     C                   IF        *in50 = *off
048900100629     C                   EVAL      PiStr=%trim(%subst(vindta:10:7))
049000100525     C                   EXSR      CHKNUM
049100100525     C                   IF        PiInt=*on
049200100525     C                   Z-ADD     PiVal         VABNSP
049300100525     C                   Z-ADD     PiVal         VATNSP
049400100525     C                   ELSE
049500100525     C                   SETON                                        31
049600100525     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049700100525     C                             + ' ' + 'VABNSP VATNSP'
049800100525     C                   ENDIF
049900100525     C                   ENDIF
050000100525     C* RMN
050100100629     C                   EVAL      PiStr=%trim(%subst(vindta:10:7))
050200100629     C                   EXSR      CHKNUM
050300100629     C                   IF        PiInt=*on
050400100629     C                   Z-ADD     PiVal         VABRMN
050500100629     C                   ELSE
050600100629     C                   SETON                                        32
050700100629     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050800100629     C                             + ' ' + 'VABRMN'
050900100629     C                   ENDIF
051000100503     C* CAD
051100100629     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
051200100629     C                   EXSR      CHKNUM
051300100629     C                   IF        PiInt=*on
051400100629     C                   Z-ADD     PiVal         Num5_0
051500100629     C                   MOVEL     Num5_0        VABCAD
051600100629     C                   ELSE
051700100629     C                   SETON                                        32
051800100629     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051900100629     C                             + ' ' + 'VABCAD'
052000100629     C                   ENDIF
052100100503     C* NCL
052200100629     C                   EVAL      PiStr=%trim(%subst(vindta:114:5))
052300100629     C                   EXSR      CHKNUM
052400100629     C                   IF        PiInt=*on
052500100629     C                   Z-ADD     PiVal         VABNCL
052600100629     C                   ELSE
052700100629     C                   SETON                                        32
052800100629     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052900100629     C                             + ' ' + 'VABNCL'
053000100629     C                   ENDIF
053100100503     C* PKB
053200100629     C                   EVAL      PiStr=%trim(%subst(vindta:119:5))
053300100629     C                   EXSR      CHKNUM
053400100629     C                   IF        PiNum=*on
053500100629     C                   Z-ADD     PiVal         VABPKB
053600100629     C                   ELSE
053700100629     C                   SETON                                        32
053800100629     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053900100629     C                             + ' ' + 'VABPKB'
054000100629     C                   ENDIF
054100100503     C* CAS
054200100629     C                   IF        %trim(%subst(vindta:125:9))<>*blanks
054300100629     C                             AND
054400100629     C                             %trim(%subst(vindta:125:9))<>'000000000'
054500100629     C                             AND
054600100629     C                             %trim(%subst(vindta:125:9))<>'0'
054700100629     C                   MOVEL     '1'           FlgCAS
054800100629     C                   EVAL      PiStr=%trim(%subst(vindta:125:9))
054900100629     C                   IF        VABVCA=*blanks
055000100629     C                   EVAL      VABVCA='EUR'
055100100629     C                   ENDIF
055200100629     C                   EXSR      CHKNUM
055300100629     C                   IF        PiNum=*on
055400100629     C                   EVAL      PiVal=PiVal / 100                            * gestisco 2 dec.
055500100629     C                   Z-ADD     PiVal         VABCAS
055600100629     C                   ELSE
055700100629     C                   SETON                                        32
055800100629     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055900100629     C                             + ' ' + 'VABCAS'
056000100629     C                   ENDIF
056100100629     C                   ENDIF
056200100310     C*
056300100310     C* Se provincia nn valorizzata la reperisco
056400100310     C* tramite TISI95R a seconda dei dati d instradamento presenti
056500100629     C                   IF        VABNZD  = *blanks AND
056600100629     C                             VABPRD  = *blanks AND
056700100629     C                             VABCAD <> *blanks
056800100629     C                   CLEAR                   TISI95DS
056900100629     C                   EVAL      I95TCN = '3'
057000100629     C                   Z-ADD     datcor        I95DAT
057100100629     C                   EVAL      I95NAR = VABNZD
057200100629     C                   EVAL      I95CAP = VABCAD
057300100629     C                   EVAL      I95LOC = VABLOD
057400100629     C                   CALL      'TISI95R'
057500100629     C                   PARM                    TISI95DS
057600100629     C                   EVAL      VABPRD = O95PRV
057700100629     C                   ENDIF
057800070730     C*
057900070730     C* Considerazioni finali su CBO/CAS
058000100629     C                   IF        FlgCAS = '1'
058100100629     C                   IF        VABCBO = '1'
058200100629     C                   EVAL      VABCBO = '4'
058300100629     C                   ENDIF
058400100629     C                   IF        VABCBO = '2'
058500100629     C                   EVAL      VABCBO = '6'
058600100629     C                   ENDIF
058700100629     C                   ENDIF
058800010202     C*
058900000801     C* Ebbene...
059000000801     C                   ADD       1             §CTRMO
059100070530     C                   IF        *in31 <> *zeros OR
059200070530     C                             *in32 <> *zeros
059300000801     C                   ADD       1             §CTRNO
059400000801     C                   EVAL      recko = vindta
059500000801     C                   EXCEPT    rigadett
059600020725     C                   EVAL      x_vinflg = '2'
059700000801     C                   ELSE
059800010201     C                   ADD       1             §CTROKVB
059900000801     C                   ENDIF
060000000801     C*
060100000801     C                   ENDSR
060200100127     C*----------------------------------------------------*
060300100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
060400100127     C*----------------------------------------------------*
060500100127     C     WRIVAT_A      BEGSR
060600100127     C*
060700100127     C* Valorizzo l buffer di scrittura del FIVAT
060800100127     C* - TIPO RECORD "A"
060900100412     C***                eval      VATTRC = 'A'
061000100412     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
061100100412     C***                exsr      wrivat
061200100127     C*
061300100127     C                   ENDSR
061400010201     C*----------------------------------------------------*
061500071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
061600010201     C*----------------------------------------------------*
061700071003     C     WRIVAT_B      BEGSR
061800010201     C*
061900021113     C* Valorizzo l buffer di scrittura del FIVAT
062000070928     C* - TIPO RECORD "B"
062100100412     C***                eval      VATTRC = 'B'
062200100412     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
062300100412     C***                exsr      wrivat
062400010201     C*
062500010201     C                   ENDSR
062600071003     C*----------------------------------------------------*
062700071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
062800071003     C*----------------------------------------------------*
062900071003     C     WRIVAT_E      BEGSR
063000071003     C*
063100071003     C* Valorizzo l buffer di scrittura del FIVAT
063200071003     C* - TIPO RECORD "E"
063300100629     C***                eval      VATTRC = 'E'
063400100629     C***                eval      VATNOT = %trim(%subst(vindta:14:6))
063500100629     C***                exsr      wrivat
063600071003     C*
063700071003     C                   ENDSR
063800100127     C*----------------------------------------------------*
063900100127     C*  SCARICO BUFFER FIVAT
064000100127     C*----------------------------------------------------*
064100100127     C     WRIVAT        BEGSR
064200100127     C*
064300100127     C                   if        vatNOT <> *blanks
064400100127     C                   ADD       1             §CTROKVT
064500100127     C                   write     FIVAT000
064600100127     C                   endif
064700100127     C*
064800100127     C                   ENDSR
064900020904
065000020904
065100010202     C*----------------------------------------------------*
065200100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
065300010202     C*----------------------------------------------------*
065400100525     C     PREVASX       BEGSR
065500100525     C*
065600021113     C* Compongo il nome del membro da dare al FIVATWWR
065700010202     C                   eval      parmbr = vlrhdl
065800010202     C                   movel     'M'           parmbr
065900070530     C                   eval      parccm = %subst(vlrKSC:2:7)
066000010202     C                   eval      paropz = '1'
066100100525     C*
066200010202     C* Effettuo la chiamata al CLLE preposto
066300100525     C  N51              call(e)   'TITVVTC'
066400010202     C                   parm                    parccm
066500010202     C                   parm                    parmbr
066600010202     C                   parm                    paropz
066700100525     C   51              call(e)   'TITVVBC'
066800100525     C                   parm                    parccm
066900100525     C                   parm                    parmbr
067000100525     C                   parm                    paropz
067100100525     C*
067200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
067300010202     C                   if        %error
067400010202     C                   movel     '1'           chkcall
067500010202     C                   else
067600010202     C                   movel     '0'           chkcall
067700010202     C                   endif
067800010202     C*
067900010202     C                   ENDSR
068000000801     C*----------------------------------------------------*
068100000801     C*  CONTROLLO NUMERICITA' CAMPI
068200000801     C*----------------------------------------------------*
068300000801     C     CHKNUM        BEGSR
068400081113     C*
068500081113     C                   IF        PiDecChr = *blanks
068600100503     C                   EVAL      PiDecChr = '.'
068700081113     C                   ENDIF
068800091223     C*
068900081113     C                   callp(e)  UBISNUM_Check(PiStr
069000081113     C                                          :PiDecChr
069100081113     C                                          :PiVal
069200081113     C                                          :PiNum
069300081113     C                                          :PiInt)
069400081113     C*
069500000801     C                   IF        %error
069600000801     C                   EVAL      PiInt=*off
069700000801     C                   ENDIF
069800000801     C*
069900000801     C                   ENDSR
070000000801     C***
070100011113
070200011113
070300000801
070400000801
070500990920      /TITLE Invio dei dati al punto operativo.
070600010202     C     invio         BEGSR
070700990920     C*
070800021113     C* 1° invio FIVAT
070900010201     C                   reset                   dscmz
071000010201     C                   move      vlrpoi        cmzdst
071100021113     C                   eval      cmzfld = 'FIVATWWR'
071200010201     C                   eval      cmzmbd = vlrhdl
071300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
071400021009     C***                if        prmfir = *blanks
071500021113     C                   eval      cmzfla = 'FIVAT00F'
071600021113     C                   eval      cmzmba = 'FIVAT00F'
071700021009     C***                else
071800021009     C***                eval      cmzfla = prmfir
071900021009     C***                eval      cmzmba = prmfir
072000021009     C***                endif
072100010201     C                   eval      cmznrr = *zeros
072200020305     C                   move      §ctrokvt      cmznrr
072300021018     C                   eval      cmzlba = vlrfl1
072400010201     C                   call(e)   'TIS711C'
072500010201     C                   parm                    dscmz
072600010201     C                   parm      *blanks       esito
072700010205     C                   if        %error
072800010205     C                             or cmzerr = '1'
072900010205     C                             or esito  = '1'
073000010205     C                   eval      wrkesito = '3'
073100010205     C                   else
073200010201     C*
073300021113     C* 2° invio FIVAB
073400100525     C                   if        *in51 = *off
073500010201     C                   reset                   dscmz
073600010201     C                   move      vlrpoi        cmzdst
073700010201     C                   eval      cmzfld = vlrfou
073800010201     C                   eval      cmzmbd = vlrhdl
073900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074000021009     C***                if        prmfir = *blanks
074100021113     C                   eval      cmzfla = 'FIVAB00F'
074200021113     C                   eval      cmzmba = 'FIVAB00F'
074300021009     C***                else
074400021009     C***                eval      cmzfla = prmfir
074500021009     C***                eval      cmzmba = prmfir
074600021009     C***                endif
074700010201     C                   eval      cmznrr = *zeros
074800010201     C                   move      §ctrokvb      cmznrr
074900021018     C                   eval      cmzlba = vlrfl1
075000010201     C                   call(e)   'TIS711C'
075100010201     C                   parm                    dscmz
075200010201     C                   parm      *blanks       esito
075300010201     C                   if        %error
075400010201     C                             or cmzerr = '1'
075500010201     C                             or esito  = '1'
075600010201     C                   eval      wrkesito = '3'
075700010201     C                   endif
075800100525     C                   endif
075900010205     C                   endif
076000990920     C*
076100000613     C                   ENDSR
076200000613     C***
076300070411
076400070411     C     *pssr         BEGSR
076500070411     C*
076600070411     C                   if        %open(tivin00r)
076700070411     C                   close     tivin00r
076800070411     C                   endif
076900070411     C                   if        %open(fivabwwr)
077000070411     C                   close     fivabwwr
077100070411     C                   endif
077200070411     C                   if        %open(fivatwwr)
077300070411     C                   close     fivatwwr
077400070411     C                   endif
077500070411     C*
077600070411     C* Effettuo la chiamata al CLLE preposto
077700100525     C  N51              call(e)   'TITVVTC'
077800100525     C                   parm                    parccm
077900100525     C                   parm                    parmbr
078000100525     C                   parm      '2'           paropz
078100100525     C   51              call(e)   'TITVVBC'
078200100525     C                   parm                    parccm
078300100525     C                   parm                    parmbr
078400100525     C                   parm      '2'           paropz
078500070411     C*
078600070411     C                   eval      wrkesito = '2'
078700070411     C*
078800070411     C                   seton                                        LR
078900070411     C*
079000070411     C                   ENDSR     '*CANCL'
079100070411     C***
079200070411
079300990910
079400000613     C     *inzsr        BEGSR
079500990910     C*
079600990910     C     *entry        plist
079700990920     C                   parm                    tivlrds
079800990921     C                   parm      wrkesito      esito
079900000724     C                   parm                    prmlit
080000000710     C                   parm                    prmfir
080100000613     C*
080200000830     C* CALCOLA LA DATA CORRENTE
080300091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
080400091223     C                   eval      datcor = %dec(%date() : *ISO)
080500000830     C*
080600000613     C                   ENDSR
080700000613     C***
080800990908
080900100629     Otitv1y4p  E            riepilogo         2
081000990915     O                                              'Upload via Internet'
081100990915     O                                           +1 'Traduzione TIVIN00R -'
081200021113     O                                           +1 'FIVABWWR/FIVATWWR'
081300010201     O                                           +1 'Report testate bolle'
081400990915     O          E            riepilogo   2
081500990915     O                       wrkdata
081600990915     O                       wrkora              +1
081700990915     O                       procname            +1
081800990915     O          E            riepilogo   2
081900990915     O                                              'Cliente..................:'
082000990915     O                       VABCCM        z     +1
082100990915     O          E            riepilogo   2
082200990920     O                                              'Riferimento Strategi.....:'
082300990920     O                       vlrhdl              +1
082400990915     O          E            riepilogo   2
082500990915     O                                              'Giusti...................:'
082600010201     O                       §CTROKVB      2   +  1
082700100525     O                       §CTROKVT      2   +  1
082800990915     O          E            riepilogo   2
082900990915     O                                              'Sbagliati e corretti.....:'
083000971022     O                       §CTRMO        2   +  1
083100990915     O          E            riepilogo   2
083200990915     O                                              'Sbagliati e scartati.....:'
083300971022     O                       §CTRNO        2   +  1
083400000613
083500100629     Otitv1y4ps E            testdett          2
083600000613     O                                              'Upload via Internet'
083700000613     O                                           +1 'Traduzione TIVIN00R -'
083800021113     O                                           +1 'FIVABWWR/FIVATWWR'
083900010201     O                                           +1 'Report testate bolle'
084000000616     O          E            testdett    3
084100000613     O                                           +2 'N° rec'
084200000613     O                                           +3 'Anteprima contenuto'
084300000616     O          E            rigadett    2
084400000613     O                       rrnum               +2
084500000621     O                       recko               +3
084600000616     O          E            findett     2
084700000613     O                       wrkdata
084800000613     O                       wrkora              +1
084900000613     O                       procname            +1
085000000616     O          E            findett     2
085100000613     O                                              'Cliente..................:'
085200000613     O                       VABCCM        z     +1
085300000616     O          E            findett     2
085400000613     O                                              'Riferimento Strategi.....:'
085500000613     O                       vlrhdl              +1
085600000616     O          E            findett     2
085700000613     O                                              'Giusti...................:'
085800010201     O                       §CTROKVB      2   +  1
085900100525     O                       §CTROKVT      2   +  1
086000000616     O          E            findett     2
086100000613     O                                              'Sbagliati e corretti.....:'
086200000613     O                       §CTRMO        2   +  1
086300000616     O          E            findett     2
086400000613     O                                              'Sbagliati e scartati.....:'
086500000613     O                       §CTRNO        2   +  1
086600000512** CMD - COMANDI CL
086700100629OVRPRTF FILE(TITV1Y4P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
086800100629OVRPRTF FILE(TITV1Y4PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
086900100629DLTOVR FILE(TITV1Y4P TITV1Y4PS) LVL(*)
087000000512
087100000512
