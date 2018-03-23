000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600100412     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800100726     Ftitv1z1p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100100726     Ftitv1z1ps O    f  198        PRINTER usropn
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
003600100728     D depspe          s              6    INZ(*blanks)
003700100728     D curspe          s              6    INZ(*blanks)
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
004900100728     D  VINDTA                     2048
005000100728     D  VINFLG                        1
005100100728     D  VINSPE                        6
005200100728     D  VINRRN                        8  0
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
011600100726     C                   if        not %open(titv1z1ps)
011700100726     C                   open      titv1z1ps
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
014100100726     C                   if        %open(titv1z1ps)
014200000616     C                   except    findett
014300100726     C                   close     titv1z1ps
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
016200100726     C                   if        not %open(titv1z1p)
016300100726     C                   open      titv1z1p
016400990915     C                   endif
016500990915     C*
016600990915     C                   except    riepilogo
016700990915     C*
016800100726     C                   if        %open(titv1z1p)
016900100726     C                   close     titv1z1p
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
020700100726     C+ declare C1 cursor for
020800100726     C+ select vindta, vinflg, substr(vindta, 200, 6) as sped, rrn(tivin00r)
020900060223     C+ from tivin00r
021000100728     C+ where substr(vindta, 1, 4) = '0000'
021100100726     C+ union
021200100726     C+ select vindta, vinflg, substr(vindta, 5, 6) as sped, rrn(tivin00r)
021300100726     C+ from tivin00r
021400100726     C+ where substr(vindta, 1, 4) = 'NVLI'
021500100726     C+ union
021600100726     C+ select vindta, vinflg, substr(vindta, 23, 6) as sped, rrn(tivin00r)
021700100726     C+ from tivin00r
021800100726     C+ where substr(vindta, 1, 4) = '2010'
021900060223     C+ order by sped
022000020725     C+ for read only
022100020725     C/END-EXEC
022200020725     C
022300020725     C/EXEC SQL
022400020725     C+ open C1
022500020725     C/END-EXEC
022600020725     C
022700020725     C/EXEC SQL
022800070530     C+ Fetch C1 into :INPUT_DS
022900020725     C/END-EXEC
023000020725     C*
023100020725     C                   dow       sqlcod = *zeros
023200990913     C*
023300100310     C                   if        vindta > *blanks
023400000613     C                   add       1             rrnum
023500000801     C*
023600020725     C                   if        vinflg = *blanks
023700020725     C                             or vinflg = '0'
023800020725     C                             or vinflg = '2'
023900000801     C*
024000020725     C                   clear                   x_vinmsg
024100020725     C                   eval      x_vinflg = '1'
024200100728     C*
024300100728     C                   z-add     1             wGiro             1 0
024400010305     C*
024500010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
024600100728     C                   EVAL      curspe=VINSPE
024700010305     C*
024800100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
024900080923     C                   if        *in50 = *off
025000071003     C                   exsr      impvab
025100100525     C                   exsr      wrivab
025200100127     C                   exsr      wrivat_a                                     => carico VAT
025300071003     C                   exsr      wrivat_b                                     => carico VAT
025400100525     C   51              exsr      wrivat_e                                     => carico VAT
025500071003     C                   else
025600080923     C*
025700071009     C                   if        wDISK = 'D'
025800091223     C                   exsr      repNSP
025900071009     C                   exsr      impvab
026000100412     C                   exsr      wrivab
026100100127     C                   exsr      wrivat_a                                     => carico VAT
026200071009     C                   exsr      wrivat_b                                     => carico VAT
026300071009     C                   exsr      wrivat_e                                     => carico VAT
026400071009     C                   else
026500071009     C*
026600010305     C                   if        depspe = *blanks                             => 1° giro
026700010305     C                   eval      depspe = curspe                              => memorizz. spediz
026800080117     C                   clear                   tivinds
026900100728     C***                exsr      repNSP
027000020305     C                   exsr      impvab
027100100127     C                   exsr      wrivat_a                                     => carico VAT
027200071003     C                   exsr      wrivat_b                                     => carico VAT
027300071003     C   50              exsr      wrivat_e                                     => carico VAT
027400010305     C                   else
027500020725     C                   if        depspe <> curspe                             => rottura di spediz
027600010305     C                   eval      depspe = curspe                              => memorizz. spediz
027700100412     C                   exsr      wrivab
027800080117     C                   clear                   tivinds
027900100728     C***                exsr      repNSP
028000020305     C                   exsr      impvab
028100100127     C                   exsr      wrivat_a                                     => carico VAT
028200071003     C                   exsr      wrivat_b                                     => carico VAT
028300071003     C   50              exsr      wrivat_e                                     => carico VAT
028400020305     C                   else                                                   => x stessa spediz
028500100401     C*
028600100728     C* Se nn richiesta esclusione spedizioni "duplicate"
028700100728     C                   select
028800100728     C                   when      wDUPREC = *blanks
028900020305     C                   exsr      impvab
029000100127     C                   exsr      wrivat_a                                     => carico VAT
029100071003     C                   exsr      wrivat_b                                     => carico VAT
029200071003     C   50              exsr      wrivat_e                                     => carico VAT
029300100728     C                   when      wDUPREC = 'C'
029400100728     C   50              exsr      wrivat_e                                     => carico VAT
029500100728     C                   endsl
029600010305     C                   endif
029700010305     C                   endif
029800010305     C                   endif
029900071003     C                   endif
030000071009     C                   endif
030100000905     C*
030200000905     C                   else
030300020725     C                   eval      x_vinflg = '1'
030400000905     C                   endif
030500000905     C*
030600020725     C     VINRRN        chain     tivin000
030700020725     C                   if        %found(tivin00r)
030800020725     C                   eval      y_vinflg = x_vinflg
030900020725     C                   eval      y_vinmsg = x_vinmsg
031000020725     C                   update    tivin000
031100020725     C                   endif
031200020725     C*
031300020725     C/EXEC SQL
031400070530     C+ Fetch C1 into :INPUT_DS
031500020725     C/END-EXEC
031600020725     C*
031700020725     C                   enddo
031800020725     C*
031900020725     C/EXEC SQL
032000020725     C+ close C1
032100020725     C/END-EXEC
032200000905     C*
032300020305     C* Scarico i VAB rimasti "in sospeso"
032400071009     C                   if        wDISK = 'C'
032500100412     C                   exsr      wrivab
032600071009     C                   endif
032700010202     C*
032800010202     C                   endif
032900990910
033000990910     C* Se non ci sono record con errori ...
033100000710     C                   if        §ctrno = 0
033200990910     C* ... restituisco esito OK.
033300990921     C                   eval      wrkesito = '0'
033400990910     C                   else
033500100525     C                   if        §ctrokvb > 0 OR
033600100525     C                             §ctrokvt > 0
033700990921     C                   eval      wrkesito = '1'
033800000710     C                   else
033900000710     C                   eval      wrkesito = '2'
034000990910     C                   endif
034100000710     C                   endif
034200990910     C*
034300990914     C                   if        %open(tivin00r)
034400990908     C                   close     tivin00r
034500990914     C                   endif
034600021113     C                   if        %open(fivabwwr)
034700021113     C                   close     fivabwwr
034800990914     C                   endif
034900021113     C                   if        %open(fivatwwr)
035000021113     C                   close     fivatwwr
035100010201     C                   endif
035200990910     C*
035300100525     C                   if        §ctrokvb > 0 OR
035400100525     C                             §ctrokvt > 0
035500000724     C                             and vlrpoi <> *zeros
035600010202     C                   exsr      invio
035700990920     C                   endif
035800990920     C*
035900910830     C                   ENDSR
036000000613     C***
036100100412
036200100412
036300010305
036400010305     C*----------------------------------------------------*
036500020305     C*  SCARICAMENTO BUFFER RECORDS VAB
036600010305     C*----------------------------------------------------*
036700020305     C     WRIVAB        BEGSR
036800100412     C*
036900100412     C* Gestisco l'eventuale rottura x numero spedizione
037000100412     C                   if        VABRMA = *blanks
037100100412     C                   movel(P)  VABRMN        VABRMA
037200100412     C                   endif
037300100412     C*
037400100525     C                   if        *in51 = *off and *in31 = *off
037500100525     C                   write     fivab000                                     => scarico il VAB
037600100525     C                   endif
037700010305     C*
037800010305     C                   ENDSR
037900990920
038000000801     C*----------------------------------------------------*
038100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
038200000801     C*----------------------------------------------------*
038300010201     C     INZVAR        BEGSR
038400000801     C*
038500010201     C                   Z-ADD     *zeros        Num5_0
038600020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
038700020725     C                   MOVEL     '0'           FlgCAS            1
038800000801     C*
038900000801     C                   ENDSR
039000000801     C*----------------------------------------------------*
039100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
039200000801     C*----------------------------------------------------*
039300020904     C     DEFCAM        BEGSR
039400080923     C*
039500100525     C                   SETOFF                                       5051
039600000801     C*
039700021113     C                   CLEAR                   FIVAB000
039800021113     C                   CLEAR                   FIVAT000
039900070730     C* Imposto i valori di default...
040000100728     C                   EVAL      VABCCM = 0015079
040100100728     C                   EVAL      VATCCM = 0015079
040200100728     C                   EVAL      VABLNP = 001
040300100728     C                   EVAL      VATLNP = 001
040400100525     C                   EVAL      VABCTR = 000
040500100525     C                   EVAL      VABCBO = '1'
040600070222     C* ... e poi verifico se sono stati passati come parametri
040700070222     C                   IF        vlrppt > *blanks
040800100525     C*
040900070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
041000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
041100070222     C                   EXSR      CHKNUM
041200070222     C                   IF        PiInt=*on
041300070222     C                   Z-ADD     PiVal         VABCCM
041400070222     C                   Z-ADD     PiVal         VATCCM
041500070222     C                   ENDIF
041600070222     C                   ENDIF
041700070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
041800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
041900070222     C                   EXSR      CHKNUM
042000070222     C                   IF        PiInt=*on
042100070222     C                   Z-ADD     PiVal         VABLNP
042200070222     C                   Z-ADD     PiVal         VATLNP
042300070222     C                   ENDIF
042400070222     C                   ENDIF
042500070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
042600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
042700070222     C                   EXSR      CHKNUM
042800070222     C                   IF        PiInt=*on
042900070222     C                   Z-ADD     PiVal         VABCTR
043000070222     C                   ENDIF
043100070928     C                   ENDIF
043200071009     C                   MOVEL     *blanks       wDISK             1
043300071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
043400071009     C                   IF        wDISK <> *blanks
043500070928     C                   SETON                                        50
043600070222     C                   ENDIF
043700100525     C                   IF        wDISK  = 'T'
043800100525     C                   SETOFF                                       50
043900100525     C                   SETON                                        51
044000100525     C                   ENDIF
044100100401     C                   MOVEL     *blanks       wDUPREC           1
044200100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
044300100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
044400100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
044500100525     C                   ENDIF
044600100525     C*
044700070222     C                   ENDIF
044800071009     C*
044900071009     C   50              EVAL      VABCTM = '7Q'
045000000801     C*
045100000801     C                   ENDSR
045200091223     C*----------------------------------------------------*
045300091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
045400091223     C*----------------------------------------------------*
045500091223     C     REPNSP        BEGSR
045600091223     C*
045700091223     C                   EXSR      INZVAR
045800091223     C                   EXSR      DEFCAM
045900091223     C*
046000091223     C* NSP => Stacco un numeratore da AZNUM
046100091223     C                   clear                   TRUL33DS
046200091223     C                   eval      I33OPE = *zeros
046300091223     C                   eval      I33CNU = 302
046400091223     C                   eval      I33NUM = 1
046500091223     C                   movel     TRUL33DS      KPJBU
046600091223     C                   call      'TRUL33R'
046700091223     C                   parm                    KPJBA
046800091223     C                   movel     KPJBU         TRUL33DS
046900091223     C                   if        O33ERR = *zeros
047000091223     C                   z-add     O33NRF        VABNSP
047100091223     C                   z-add     O33NRF        VATNSP
047200091223     C                   else
047300091223     C                   SETON                                        31
047400091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047500091223     C                             + ' ' + 'VABNSP VATNSP'
047600091223     C                   endif
047700091223     C*
047800091223     C                   ENDSR
047900000801     C*----------------------------------------------------*
048000021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
048100000801     C*----------------------------------------------------*
048200010201     C     IMPVAB        BEGSR
048300070530     C*
048400070530     C                   SETOFF                                       3132
048500100728     C*
048600100728     C* Gestione tipo record '0000'
048700100728     C                   IF        %subst(vindta:1:4) = '0000'
048800100728     C*
048900100728     C* Se nn primo giro => scarico il buffer precedente
049000100728     C                   if        wGiro = 1
049100100728     C                   eval      wGiro = 2
049200100728     C                   else
049300100728     C                   WRITE     FIVAB000
049400100728     C                   endif
049500100728     C*
049600100728     C* ......inizializzazioni iniziali e formati record file Bartolini
049700100728     C                   EXSR      INZVAR
049800100728     C                   EXSR      DEFCAM
049900100728     C*
050000100728     C                   MOVE(P)   vlrpoi        VABFGS
050100100728     C                   MOVE(P)   vlrpoi        VATFGS
050200100728     C*
050300100728     C                   MOVEL     datcor        VABAAS
050400100728     C                   MOVEL     datcor        VATAAS
050500100728     C                   MOVE      datcor        VABMGS
050600100728     C*
050700100728     C                   EVAL      VABRSD=%trim(%subst(vindta:6:30))
050800100503     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
050900100503     C     '@':'A'       XLATE     VABRSD        VABRSD
051000100503     C* ==
051100100728     C                   EVAL      VABIND=%trim(%subst(vindta:36:30))
051200100728     C                   EVAL      VABLOD=%trim(%subst(vindta:66:20))
051300100728     C                   EVAL      VABPRD=%trim(%subst(vindta:91:2))
051400100728     C                   EVAL      VABRMA=%trim(%subst(vindta:200:6))
051500100520     C*
051600100525     C* NSP
051700100525     C                   IF        *in50 = *off
051800100728     C                   EVAL      PiStr=VINSPE
051900100525     C                   EXSR      CHKNUM
052000100525     C                   IF        PiInt=*on
052100100525     C                   Z-ADD     PiVal         VABNSP
052200100525     C                   Z-ADD     PiVal         VATNSP
052300100525     C                   ELSE
052400100525     C                   SETON                                        31
052500100525     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052600100525     C                             + ' ' + 'VABNSP VATNSP'
052700100525     C                   ENDIF
052800100728     C*
052900100728     C                   ELSE
053000100728     C*
053100100728     C* NSP => Stacco un numeratore da AZNUM
053200100728     C                   clear                   TRUL33DS
053300100728     C                   eval      I33OPE = *zeros
053400100728     C                   eval      I33CNU = 302
053500100728     C                   eval      I33NUM = 1
053600100728     C                   movel     TRUL33DS      KPJBU
053700100728     C                   call      'TRUL33R'
053800100728     C                   parm                    KPJBA
053900100728     C                   movel     KPJBU         TRUL33DS
054000100728     C                   if        O33ERR = *zeros
054100100728     C                   z-add     O33NRF        VABNSP
054200100728     C                   z-add     O33NRF        VATNSP
054300100728     C                   else
054400100728     C                   SETON                                        31
054500100728     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054600100728     C                             + ' ' + 'VABNSP VATNSP'
054700100728     C                   endif
054800100728     C*
054900100525     C                   ENDIF
055000100525     C* RMN
055100100728     C                   EVAL      PiStr=%trim(%subst(vindta:200:6))
055200100629     C                   EXSR      CHKNUM
055300100629     C                   IF        PiInt=*on
055400100629     C                   Z-ADD     PiVal         VABRMN
055500100629     C                   ELSE
055600100629     C                   SETON                                        32
055700100629     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055800100629     C                             + ' ' + 'VABRMN'
055900100629     C                   ENDIF
056000100503     C* CAD
056100100728     C                   EVAL      PiStr=%trim(%subst(vindta:86:5))
056200100629     C                   EXSR      CHKNUM
056300100629     C                   IF        PiInt=*on
056400100629     C                   Z-ADD     PiVal         Num5_0
056500100629     C                   MOVEL     Num5_0        VABCAD
056600100629     C                   ELSE
056700100629     C                   SETON                                        32
056800100629     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056900100629     C                             + ' ' + 'VABCAD'
057000100629     C                   ENDIF
057100100503     C* NCL
057200100728     C                   EVAL      PiStr=%trim(%subst(vindta:129:5))
057300100629     C                   EXSR      CHKNUM
057400100629     C                   IF        PiInt=*on
057500100629     C                   Z-ADD     PiVal         VABNCL
057600100629     C                   ELSE
057700100629     C                   SETON                                        32
057800100629     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
057900100629     C                             + ' ' + 'VABNCL'
058000100629     C                   ENDIF
058100100503     C* PKB
058200100728     C                   EVAL      PiStr=%trim(%subst(vindta:134:4))
058300100629     C                   EXSR      CHKNUM
058400100629     C                   IF        PiNum=*on
058500100629     C                   Z-ADD     PiVal         VABPKB
058600100629     C                   ELSE
058700100629     C                   SETON                                        32
058800100629     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
058900100629     C                             + ' ' + 'VABPKB'
059000100629     C                   ENDIF
059100100310     C*
059200100310     C* Se provincia nn valorizzata la reperisco
059300100310     C* tramite TISI95R a seconda dei dati d instradamento presenti
059400100629     C                   IF        VABNZD  = *blanks AND
059500100629     C                             VABPRD  = *blanks AND
059600100629     C                             VABCAD <> *blanks
059700100629     C                   CLEAR                   TISI95DS
059800100629     C                   EVAL      I95TCN = '3'
059900100629     C                   Z-ADD     datcor        I95DAT
060000100629     C                   EVAL      I95NAR = VABNZD
060100100629     C                   EVAL      I95CAP = VABCAD
060200100629     C                   EVAL      I95LOC = VABLOD
060300100629     C                   CALL      'TISI95R'
060400100629     C                   PARM                    TISI95DS
060500100629     C                   EVAL      VABPRD = O95PRV
060600100629     C                   ENDIF
060700070730     C*
060800070730     C* Considerazioni finali su CBO/CAS
060900100629     C                   IF        FlgCAS = '1'
061000100629     C                   IF        VABCBO = '1'
061100100629     C                   EVAL      VABCBO = '4'
061200100629     C                   ENDIF
061300100629     C                   IF        VABCBO = '2'
061400100629     C                   EVAL      VABCBO = '6'
061500100629     C                   ENDIF
061600100629     C                   ENDIF
061700100728     C*
061800100728     C                   ENDIF
061900100728     C*
062000100728     C* Gestione tipo record 'NVLI'
062100100728     C                   IF        %subst(vindta:1:4) = 'NVLI'
062200100728     C*
062300100728     C                   EVAL      VABNOT=%trim(%subst(vindta:11:35))
062400100728     C                   EVAL      VABNT2=%trim(%subst(vindta:11+35:35))
062500100728     C*
062600100728     C                   ENDIF
062700010202     C*
062800000801     C* Ebbene...
062900000801     C                   ADD       1             §CTRMO
063000070530     C                   IF        *in31 <> *zeros OR
063100070530     C                             *in32 <> *zeros
063200000801     C                   ADD       1             §CTRNO
063300000801     C                   EVAL      recko = vindta
063400000801     C                   EXCEPT    rigadett
063500020725     C                   EVAL      x_vinflg = '2'
063600000801     C                   ELSE
063700010201     C                   ADD       1             §CTROKVB
063800000801     C                   ENDIF
063900000801     C*
064000000801     C                   ENDSR
064100100127     C*----------------------------------------------------*
064200100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
064300100127     C*----------------------------------------------------*
064400100127     C     WRIVAT_A      BEGSR
064500100127     C*
064600100127     C* Valorizzo l buffer di scrittura del FIVAT
064700100127     C* - TIPO RECORD "A"
064800100412     C***                eval      VATTRC = 'A'
064900100412     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
065000100412     C***                exsr      wrivat
065100100127     C*
065200100127     C                   ENDSR
065300010201     C*----------------------------------------------------*
065400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
065500010201     C*----------------------------------------------------*
065600071003     C     WRIVAT_B      BEGSR
065700010201     C*
065800021113     C* Valorizzo l buffer di scrittura del FIVAT
065900070928     C* - TIPO RECORD "B"
066000100412     C***                eval      VATTRC = 'B'
066100100412     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
066200100412     C***                exsr      wrivat
066300010201     C*
066400010201     C                   ENDSR
066500071003     C*----------------------------------------------------*
066600071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
066700071003     C*----------------------------------------------------*
066800071003     C     WRIVAT_E      BEGSR
066900071003     C*
067000071003     C* Valorizzo l buffer di scrittura del FIVAT
067100071003     C* - TIPO RECORD "E"
067200100728     C*
067300100728     C* Gestione tipo record '2010'
067400100728     C                   IF        %subst(vindta:1:4) = '2010'
067500100728     C*
067600100728     C                   eval      VATTRC = 'E'
067700100728     C                   eval      VATNOT = %trim(%subst(vindta:29:9))
067800100728     C                   exsr      wrivat
067900100728     C*
068000100728     C                   ENDIF
068100071003     C*
068200071003     C                   ENDSR
068300100127     C*----------------------------------------------------*
068400100127     C*  SCARICO BUFFER FIVAT
068500100127     C*----------------------------------------------------*
068600100127     C     WRIVAT        BEGSR
068700100127     C*
068800100127     C                   if        vatNOT <> *blanks
068900100127     C                   ADD       1             §CTROKVT
069000100127     C                   write     FIVAT000
069100100127     C                   endif
069200100127     C*
069300100127     C                   ENDSR
069400020904
069500020904
069600010202     C*----------------------------------------------------*
069700100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
069800010202     C*----------------------------------------------------*
069900100525     C     PREVASX       BEGSR
070000100525     C*
070100021113     C* Compongo il nome del membro da dare al FIVATWWR
070200010202     C                   eval      parmbr = vlrhdl
070300010202     C                   movel     'M'           parmbr
070400070530     C                   eval      parccm = %subst(vlrKSC:2:7)
070500010202     C                   eval      paropz = '1'
070600100525     C*
070700010202     C* Effettuo la chiamata al CLLE preposto
070800100525     C  N51              call(e)   'TITVVTC'
070900010202     C                   parm                    parccm
071000010202     C                   parm                    parmbr
071100010202     C                   parm                    paropz
071200100525     C   51              call(e)   'TITVVBC'
071300100525     C                   parm                    parccm
071400100525     C                   parm                    parmbr
071500100525     C                   parm                    paropz
071600100525     C*
071700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
071800010202     C                   if        %error
071900010202     C                   movel     '1'           chkcall
072000010202     C                   else
072100010202     C                   movel     '0'           chkcall
072200010202     C                   endif
072300010202     C*
072400010202     C                   ENDSR
072500000801     C*----------------------------------------------------*
072600000801     C*  CONTROLLO NUMERICITA' CAMPI
072700000801     C*----------------------------------------------------*
072800000801     C     CHKNUM        BEGSR
072900081113     C*
073000081113     C                   IF        PiDecChr = *blanks
073100100503     C                   EVAL      PiDecChr = '.'
073200081113     C                   ENDIF
073300091223     C*
073400081113     C                   callp(e)  UBISNUM_Check(PiStr
073500081113     C                                          :PiDecChr
073600081113     C                                          :PiVal
073700081113     C                                          :PiNum
073800081113     C                                          :PiInt)
073900081113     C*
074000000801     C                   IF        %error
074100000801     C                   EVAL      PiInt=*off
074200000801     C                   ENDIF
074300000801     C*
074400000801     C                   ENDSR
074500000801     C***
074600011113
074700011113
074800000801
074900000801
075000990920      /TITLE Invio dei dati al punto operativo.
075100010202     C     invio         BEGSR
075200990920     C*
075300021113     C* 1° invio FIVAT
075400010201     C                   reset                   dscmz
075500010201     C                   move      vlrpoi        cmzdst
075600021113     C                   eval      cmzfld = 'FIVATWWR'
075700010201     C                   eval      cmzmbd = vlrhdl
075800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075900021009     C***                if        prmfir = *blanks
076000021113     C                   eval      cmzfla = 'FIVAT00F'
076100021113     C                   eval      cmzmba = 'FIVAT00F'
076200021009     C***                else
076300021009     C***                eval      cmzfla = prmfir
076400021009     C***                eval      cmzmba = prmfir
076500021009     C***                endif
076600010201     C                   eval      cmznrr = *zeros
076700020305     C                   move      §ctrokvt      cmznrr
076800021018     C                   eval      cmzlba = vlrfl1
076900010201     C                   call(e)   'TIS711C'
077000010201     C                   parm                    dscmz
077100010201     C                   parm      *blanks       esito
077200010205     C                   if        %error
077300010205     C                             or cmzerr = '1'
077400010205     C                             or esito  = '1'
077500010205     C                   eval      wrkesito = '3'
077600010205     C                   else
077700010201     C*
077800021113     C* 2° invio FIVAB
077900100525     C                   if        *in51 = *off
078000010201     C                   reset                   dscmz
078100010201     C                   move      vlrpoi        cmzdst
078200010201     C                   eval      cmzfld = vlrfou
078300010201     C                   eval      cmzmbd = vlrhdl
078400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
078500021009     C***                if        prmfir = *blanks
078600021113     C                   eval      cmzfla = 'FIVAB00F'
078700021113     C                   eval      cmzmba = 'FIVAB00F'
078800021009     C***                else
078900021009     C***                eval      cmzfla = prmfir
079000021009     C***                eval      cmzmba = prmfir
079100021009     C***                endif
079200010201     C                   eval      cmznrr = *zeros
079300010201     C                   move      §ctrokvb      cmznrr
079400021018     C                   eval      cmzlba = vlrfl1
079500010201     C                   call(e)   'TIS711C'
079600010201     C                   parm                    dscmz
079700010201     C                   parm      *blanks       esito
079800010201     C                   if        %error
079900010201     C                             or cmzerr = '1'
080000010201     C                             or esito  = '1'
080100010201     C                   eval      wrkesito = '3'
080200010201     C                   endif
080300100525     C                   endif
080400010205     C                   endif
080500990920     C*
080600000613     C                   ENDSR
080700000613     C***
080800070411
080900070411     C     *pssr         BEGSR
081000070411     C*
081100070411     C                   if        %open(tivin00r)
081200070411     C                   close     tivin00r
081300070411     C                   endif
081400070411     C                   if        %open(fivabwwr)
081500070411     C                   close     fivabwwr
081600070411     C                   endif
081700070411     C                   if        %open(fivatwwr)
081800070411     C                   close     fivatwwr
081900070411     C                   endif
082000070411     C*
082100070411     C* Effettuo la chiamata al CLLE preposto
082200100525     C  N51              call(e)   'TITVVTC'
082300100525     C                   parm                    parccm
082400100525     C                   parm                    parmbr
082500100525     C                   parm      '2'           paropz
082600100525     C   51              call(e)   'TITVVBC'
082700100525     C                   parm                    parccm
082800100525     C                   parm                    parmbr
082900100525     C                   parm      '2'           paropz
083000070411     C*
083100070411     C                   eval      wrkesito = '2'
083200070411     C*
083300070411     C                   seton                                        LR
083400070411     C*
083500070411     C                   ENDSR     '*CANCL'
083600070411     C***
083700070411
083800990910
083900000613     C     *inzsr        BEGSR
084000990910     C*
084100990910     C     *entry        plist
084200990920     C                   parm                    tivlrds
084300990921     C                   parm      wrkesito      esito
084400000724     C                   parm                    prmlit
084500000710     C                   parm                    prmfir
084600000613     C*
084700000830     C* CALCOLA LA DATA CORRENTE
084800091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
084900091223     C                   eval      datcor = %dec(%date() : *ISO)
085000000830     C*
085100000613     C                   ENDSR
085200000613     C***
085300990908
085400100726     Otitv1z1p  E            riepilogo         2
085500990915     O                                              'Upload via Internet'
085600990915     O                                           +1 'Traduzione TIVIN00R -'
085700021113     O                                           +1 'FIVABWWR/FIVATWWR'
085800010201     O                                           +1 'Report testate bolle'
085900990915     O          E            riepilogo   2
086000990915     O                       wrkdata
086100990915     O                       wrkora              +1
086200990915     O                       procname            +1
086300990915     O          E            riepilogo   2
086400990915     O                                              'Cliente..................:'
086500990915     O                       VABCCM        z     +1
086600990915     O          E            riepilogo   2
086700990920     O                                              'Riferimento Strategi.....:'
086800990920     O                       vlrhdl              +1
086900990915     O          E            riepilogo   2
087000990915     O                                              'Giusti...................:'
087100010201     O                       §CTROKVB      2   +  1
087200100525     O                       §CTROKVT      2   +  1
087300990915     O          E            riepilogo   2
087400990915     O                                              'Sbagliati e corretti.....:'
087500971022     O                       §CTRMO        2   +  1
087600990915     O          E            riepilogo   2
087700990915     O                                              'Sbagliati e scartati.....:'
087800971022     O                       §CTRNO        2   +  1
087900000613
088000100726     Otitv1z1ps E            testdett          2
088100000613     O                                              'Upload via Internet'
088200000613     O                                           +1 'Traduzione TIVIN00R -'
088300021113     O                                           +1 'FIVABWWR/FIVATWWR'
088400010201     O                                           +1 'Report testate bolle'
088500000616     O          E            testdett    3
088600000613     O                                           +2 'N° rec'
088700000613     O                                           +3 'Anteprima contenuto'
088800000616     O          E            rigadett    2
088900000613     O                       rrnum               +2
089000000621     O                       recko               +3
089100000616     O          E            findett     2
089200000613     O                       wrkdata
089300000613     O                       wrkora              +1
089400000613     O                       procname            +1
089500000616     O          E            findett     2
089600000613     O                                              'Cliente..................:'
089700000613     O                       VABCCM        z     +1
089800000616     O          E            findett     2
089900000613     O                                              'Riferimento Strategi.....:'
090000000613     O                       vlrhdl              +1
090100000616     O          E            findett     2
090200000613     O                                              'Giusti...................:'
090300010201     O                       §CTROKVB      2   +  1
090400100525     O                       §CTROKVT      2   +  1
090500000616     O          E            findett     2
090600000613     O                                              'Sbagliati e corretti.....:'
090700000613     O                       §CTRMO        2   +  1
090800000616     O          E            findett     2
090900000613     O                                              'Sbagliati e scartati.....:'
091000000613     O                       §CTRNO        2   +  1
091100000512** CMD - COMANDI CL
091200100726OVRPRTF FILE(TITV1Z1P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
091300100726OVRPRTF FILE(TITV1Z1PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
091400100726DLTOVR FILE(TITV1Z1P TITV1Z1PS) LVL(*)
091500000512
091600000512
