000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400100607     Ftabel00f  if   e           k disk
000500020724     Fazorg01l  iF   e           k DISK
000600020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000700100412     FFIVABwwr  O    E             DISK    usropn
000800021113     FFIVATwwr  O    E             DISK    usropn
000900100607     Ftitv1y2p  O    f  132        PRINTER usropn
001000000621     F                                     oflind(*inoa)
001100070411     F                                     infsr(*pssr)
001200100607     Ftitv1y2ps O    f  198        PRINTER usropn
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
002700070730     D tisi95ds      e ds
002800100607     D ds15          e ds
002900990910     D esito           s              1
003000000724     D prmlit          s             10
003100000710     D prmfir          s             10
003200990921     D wrkesito        s                   like(esito)
003300990915     D wrkdata         s               d
003400990915     D wrkora          s               t
003500000613     D rrnum           s              6  0 INZ(*zeros)
003600000621     D recko           s            150    INZ(*blanks)
003700011113     D depcmd          s            150    INZ(*blanks)
003800100607     D depspe          s              8    INZ(*blanks)
003900100607     D curspe          s              8    INZ(*blanks)
004000010202     D parccm          s              8    INZ(*blanks)
004100010202     D parmbr          s             10    INZ(*blanks)
004200010202     D paropz          s              1    INZ(*blanks)
004300010202     D chkcall         s              1    INZ(*blanks)
004400080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
004500091223     D Num5_0          s              5  0
004600100607
004700100607     D*------------
004800100607     D* GESTIONE NAZIONI ESTERE
004900100607     D*------------
005000100607     D wNAZ            S              3    INZ
005100100607     D jNAZ            S              5  0 INZ
005200100607     D skNAZISO        S              3    DIM(1000)
005300100607     D skNAZBAR        S              3    DIM(1000)
005400100607
005500100607
005600100607     D*------------------
005700100607     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
005800100607     D*------------------
005900110616     D InStringa       S          65535A   VARYING                              (stringa input)
006000100607     D InSepara        S             10A                                        (separatore)
006100100607     D InTypeOut       S              1A                                        (tipo output)
006200110617     D wSkParam        S          65535A                                        (output)
006300100607     D OutErrore       S              1A                                        (flag errore)
006400100607     D OutMsg          S             80A                                        (messaggio errore)
006500100607     D SkSplitCSV_6    S            512    DIM(128)
006600020725
006700020725     D*------------------
006800020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
006900020725     D*------------------
007000070530     D INPUT_DS        DS                  INZ
007100020725     D  VINDTA                 1   2048
007200020725     D  VINFLG              2049   2049
007300020725     D  VINSPE              2050   2059
007400020725     D  VINRRN              2060   2067  0
007500020725     D*
007600081113
007700091223     D*------------------
007800091223     D* DS REPERIMENTO NUMERATORE
007900091223     D*------------------
008000100309     D trul33ds      e ds                  inz
008100091223     D*------------------
008200091223     D* DS ARCHITETTURA
008300091223     D*------------------
008400091223     D kpjba         e ds                  inz
008500091223
008600081113
008700081113     D*------------------
008800081113     D* LINKING A DEFINIZIONI ESTERNE
008900081113     D*------------------
009000081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009100090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009200081113
009300990908
009400010201
009500010201
009600080117     C*
009700080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
009800080117     C
009900080117     C/EXEC SQL
010000080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
010100080117     C/END-EXEC
010200100607     C*
010300100607     C* Carico le tabelle
010400100607     C                   exsr      cartab
010500100607     C*
010600990915     C                   time                    wrkdata
010700990915     C                   time                    wrkora
010800000913     C                   reset                   rrnum
010900990921     C                   reset                   esito
011000990921     C                   reset                   wrkesito
011100000724     C*
011200000724     C* SE OCCORRE SPEDIRE IN FILIALE
011300000724     C                   if        vlrpoi <> *zeros
011400000724     C*
011500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011700000724     C     vlrpoi        chain     azorg01l
011800000724     C                   if        %found
011900000616     C                   movel(p)  CMD(1)        depcmd
012000020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
012100000616     C*
012200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
012300011113     C                   Z-ADD     150           LENGH            15 5
012400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012500000616     C                   PARM                    depcmd
012600000616     C                   PARM                    LENGH
012700000724     C*
012800000724     C                   endif
012900000724     C                   endif
013000000616     C*
013100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013200000616     C                   movel(p)  CMD(2)        depcmd
013300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
013400000616     C*
013500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
013600011113     C                   Z-ADD     150           LENGH            15 5
013700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013800000616     C                   PARM                    depcmd
013900000616     C                   PARM                    LENGH
014000000616     C*
014100100607     C                   if        not %open(titv1y2ps)
014200100607     C                   open      titv1y2ps
014300000616     C                   except    testdett
014400000613     C                   endif
014500000613     C*
014600100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
014700070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
014800000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014900000613     C*
015000010202     C* Effettuo la chiamata al CLLE preposto
015100100525     C  N51              call(e)   'TITVVTC'
015200100525     C                   parm                    parccm
015300100525     C                   parm                    parmbr
015400100525     C                   parm      '2'           paropz
015500100525     C   51              call(e)   'TITVVBC'
015600100525     C                   parm                    parccm
015700100525     C                   parm                    parmbr
015800100525     C                   parm      '2'           paropz
015900070730     C*
016000070730     C* Effettuo lancio TISI95 solo x chiusura
016100070730     C                   CLEAR                   TISI95DS
016200070730     C                   EVAL      I95TLA = 'C'
016300070730     C                   CALL      'TISI95R'
016400070730     C                   PARM                    TISI95DS
016500100607     C*
016600100607     C* Effettuo lancio XSPLIT solo x chiusura
016700100607     C                   CALL(e)   'XSPLIT2'
016800110616     C                   PARM      ''            InStringa
016900100607     C                   PARM                    InSepara
017000100607     C                   PARM                    InTypeOut
017100110616     C                   PARM      ''            wSkParam
017200100607     C                   PARM                    OutErrore
017300100607     C                   PARM                    OutMsg
017400010202     C*
017500100607     C                   if        %open(titv1y2ps)
017600000616     C                   except    findett
017700100607     C                   close     titv1y2ps
017800000613     C                   endif
017900000616     C*
018000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018100011113     C                   Z-ADD     150           LENGH            15 5
018200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
018300000616     C                   PARM                    CMD(3)
018400000616     C                   PARM                    LENGH
018500000616     C*
018600000801     C
018700010201     C                   seton                                        LR
018800000613
018900100310
019000100310
019100000613     C*--------------------------------------------------------
019200000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
019300000613     C*--------------------------------------------------------
019400000621     C     STPR          BEGSR
019500000613     C*
019600100607     C                   if        not %open(titv1y2p)
019700100607     C                   open      titv1y2p
019800990915     C                   endif
019900990915     C*
020000990915     C                   except    riepilogo
020100990915     C*
020200100607     C                   if        %open(titv1y2p)
020300100607     C                   close     titv1y2p
020400990914     C                   endif
020500990910     C*
020600000613     C                   ENDSR
020700000613     C***
020800990908
020900000801
021000910830     C*--------------------------------------------------------
021100070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
021200910830     C*--------------------------------------------------------
021300070530     C     RWFILE        BEGSR
021400990910     C*
021500990914     C                   if        not %open(tivin00r)
021600990908     C                   open      tivin00r
021700990914     C                   endif
021800070530     C*
021900100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
022000100525     C                   exsr      prevasx
022100010201     C*
022200010202     C                   if        chkcall = '0'
022300010202     C*
022400100525     C                   if        not %open(fivabwwr)
022500100525     C                   open      fivabwwr
022600100525     C                   endif
022700100525     C*
022800021113     C                   if        not %open(fivatwwr)
022900021113     C                   open      fivatwwr
023000010201     C                   endif
023100080117     C*
023200080117     C                   EXSR      INZVAR
023300080117     C                   EXSR      DEFCAM
023400990910     C*
023500010201     C                   clear                   §CTROKVB          5 0
023600020305     C                   clear                   §CTROKVT          5 0
023700000801     C                   clear                   §CTRMO            5 0
023800000801     C                   clear                   §CTRNO            5 0
023900990910     C*
024000020725     C/EXEC SQL
024100020725     C+ declare C1 cursor for select
024200100607     C+ vindta, vinflg, substr(vindta, 16, 8) as sped, rrn(tivin00r)
024300060223     C+ from tivin00r
024400060223     C+ order by sped
024500020725     C+ for read only
024600020725     C/END-EXEC
024700020725     C
024800020725     C/EXEC SQL
024900020725     C+ open C1
025000020725     C/END-EXEC
025100020725     C
025200020725     C/EXEC SQL
025300070530     C+ Fetch C1 into :INPUT_DS
025400020725     C/END-EXEC
025500020725     C*
025600020725     C                   dow       sqlcod = *zeros
025700990913     C*
025800100310     C                   if        vindta > *blanks
025900000613     C                   add       1             rrnum
026000000801     C*
026100020725     C                   if        vinflg = *blanks
026200020725     C                             or vinflg = '0'
026300020725     C                             or vinflg = '2'
026400000801     C*
026500020725     C                   clear                   x_vinmsg
026600020725     C                   eval      x_vinflg = '1'
026700010305     C*
026800010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
026900100607     C                   EVAL      curspe=%trim(%subst(vindta:16:8))
027000010305     C*
027100100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
027200080923     C                   if        *in50 = *off
027300071003     C                   exsr      impvab
027400100525     C                   exsr      wrivab
027500100127     C                   exsr      wrivat_a                                     => carico VAT
027600071003     C                   exsr      wrivat_b                                     => carico VAT
027700100525     C   51              exsr      wrivat_e                                     => carico VAT
027800071003     C                   else
027900080923     C*
028000071009     C                   if        wDISK = 'D'
028100091223     C                   exsr      repNSP
028200071009     C                   exsr      impvab
028300100412     C                   exsr      wrivab
028400100127     C                   exsr      wrivat_a                                     => carico VAT
028500071009     C                   exsr      wrivat_b                                     => carico VAT
028600071009     C                   exsr      wrivat_e                                     => carico VAT
028700071009     C                   else
028800071009     C*
028900010305     C                   if        depspe = *blanks                             => 1° giro
029000010305     C                   eval      depspe = curspe                              => memorizz. spediz
029100080117     C                   clear                   tivinds
029200091223     C                   exsr      repNSP
029300020305     C                   exsr      impvab
029400100127     C                   exsr      wrivat_a                                     => carico VAT
029500071003     C                   exsr      wrivat_b                                     => carico VAT
029600071003     C   50              exsr      wrivat_e                                     => carico VAT
029700010305     C                   else
029800020725     C                   if        depspe <> curspe                             => rottura di spediz
029900010305     C                   eval      depspe = curspe                              => memorizz. spediz
030000100412     C                   exsr      wrivab
030100080117     C                   clear                   tivinds
030200091223     C                   exsr      repNSP
030300020305     C                   exsr      impvab
030400100127     C                   exsr      wrivat_a                                     => carico VAT
030500071003     C                   exsr      wrivat_b                                     => carico VAT
030600071003     C   50              exsr      wrivat_e                                     => carico VAT
030700020305     C                   else                                                   => x stessa spediz
030800100401     C*
030900100401     C* Se nn richiesta esclusione spedizioni "duplicate"
031000100412     C                   if        wDUPREC <> 'N'
031100020305     C                   exsr      impvab
031200100127     C                   exsr      wrivat_a                                     => carico VAT
031300071003     C                   exsr      wrivat_b                                     => carico VAT
031400071003     C   50              exsr      wrivat_e                                     => carico VAT
031500100401     C                   endif
031600010305     C                   endif
031700010305     C                   endif
031800010305     C                   endif
031900071003     C                   endif
032000071009     C                   endif
032100000905     C*
032200000905     C                   else
032300020725     C                   eval      x_vinflg = '1'
032400000905     C                   endif
032500000905     C*
032600020725     C     VINRRN        chain     tivin000
032700020725     C                   if        %found(tivin00r)
032800020725     C                   eval      y_vinflg = x_vinflg
032900020725     C                   eval      y_vinmsg = x_vinmsg
033000020725     C                   update    tivin000
033100020725     C                   endif
033200020725     C*
033300020725     C/EXEC SQL
033400070530     C+ Fetch C1 into :INPUT_DS
033500020725     C/END-EXEC
033600020725     C*
033700020725     C                   enddo
033800020725     C*
033900020725     C/EXEC SQL
034000020725     C+ close C1
034100020725     C/END-EXEC
034200000905     C*
034300020305     C* Scarico i VAB rimasti "in sospeso"
034400071009     C                   if        wDISK = 'C'
034500100412     C                   exsr      wrivab
034600071009     C                   endif
034700010202     C*
034800010202     C                   endif
034900990910
035000990910     C* Se non ci sono record con errori ...
035100000710     C                   if        §ctrno = 0
035200990910     C* ... restituisco esito OK.
035300990921     C                   eval      wrkesito = '0'
035400990910     C                   else
035500100525     C                   if        §ctrokvb > 0 OR
035600100525     C                             §ctrokvt > 0
035700990921     C                   eval      wrkesito = '1'
035800000710     C                   else
035900000710     C                   eval      wrkesito = '2'
036000990910     C                   endif
036100000710     C                   endif
036200990910     C*
036300990914     C                   if        %open(tivin00r)
036400990908     C                   close     tivin00r
036500990914     C                   endif
036600021113     C                   if        %open(fivabwwr)
036700021113     C                   close     fivabwwr
036800990914     C                   endif
036900021113     C                   if        %open(fivatwwr)
037000021113     C                   close     fivatwwr
037100010201     C                   endif
037200990910     C*
037300100525     C                   if        §ctrokvb > 0 OR
037400100525     C                             §ctrokvt > 0
037500000724     C                             and vlrpoi <> *zeros
037600010202     C                   exsr      invio
037700990920     C                   endif
037800990920     C*
037900910830     C                   ENDSR
038000000613     C***
038100100412
038200100412
038300010305
038400010305     C*----------------------------------------------------*
038500020305     C*  SCARICAMENTO BUFFER RECORDS VAB
038600010305     C*----------------------------------------------------*
038700020305     C     WRIVAB        BEGSR
038800100412     C*
038900100412     C* Gestisco l'eventuale rottura x numero spedizione
039000100412     C                   if        VABRMA = *blanks
039100100412     C                   movel(P)  VABRMN        VABRMA
039200100412     C                   endif
039300100412     C*
039400100525     C                   if        *in51 = *off and *in31 = *off
039500100525     C                   write     fivab000                                     => scarico il VAB
039600100525     C                   endif
039700010305     C*
039800010305     C                   ENDSR
039900990920
040000000801     C*----------------------------------------------------*
040100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
040200000801     C*----------------------------------------------------*
040300010201     C     INZVAR        BEGSR
040400000801     C*
040500010201     C                   Z-ADD     *zeros        Num5_0
040600020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
040700020725     C                   MOVEL     '0'           FlgCAS            1
040800000801     C*
040900000801     C                   ENDSR
041000000801     C*----------------------------------------------------*
041100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
041200000801     C*----------------------------------------------------*
041300020904     C     DEFCAM        BEGSR
041400080923     C*
041500100525     C                   SETOFF                                       5051
041600000801     C*
041700021113     C                   CLEAR                   FIVAB000
041800021113     C                   CLEAR                   FIVAT000
041900100607     C                   CLEAR                   SkSplitCSV_6
042000070730     C* Imposto i valori di default...
042100100607     C                   EVAL      VABCCM = 0661313
042200100607     C                   EVAL      VATCCM = 0661313
042300100607     C                   EVAL      VABLNP = 066
042400100607     C                   EVAL      VATLNP = 066
042500100525     C                   EVAL      VABCTR = 000
042600100525     C                   EVAL      VABCBO = '1'
042700070222     C* ... e poi verifico se sono stati passati come parametri
042800070222     C                   IF        vlrppt > *blanks
042900100525     C*
043000070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
043100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
043200070222     C                   EXSR      CHKNUM
043300070222     C                   IF        PiInt=*on
043400070222     C                   Z-ADD     PiVal         VABCCM
043500070222     C                   Z-ADD     PiVal         VATCCM
043600070222     C                   ENDIF
043700070222     C                   ENDIF
043800070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
043900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
044000070222     C                   EXSR      CHKNUM
044100070222     C                   IF        PiInt=*on
044200070222     C                   Z-ADD     PiVal         VABLNP
044300070222     C                   Z-ADD     PiVal         VATLNP
044400070222     C                   ENDIF
044500070222     C                   ENDIF
044600070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
044700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
044800070222     C                   EXSR      CHKNUM
044900070222     C                   IF        PiInt=*on
045000070222     C                   Z-ADD     PiVal         VABCTR
045100070222     C                   ENDIF
045200070928     C                   ENDIF
045300071009     C                   MOVEL     *blanks       wDISK             1
045400071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
045500071009     C                   IF        wDISK <> *blanks
045600070928     C                   SETON                                        50
045700070222     C                   ENDIF
045800100525     C                   IF        wDISK  = 'T'
045900100525     C                   SETOFF                                       50
046000100525     C                   SETON                                        51
046100100525     C                   ENDIF
046200100401     C                   MOVEL     *blanks       wDUPREC           1
046300100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
046400100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
046500100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
046600100525     C                   ENDIF
046700100525     C*
046800070222     C                   ENDIF
046900071009     C*
047000071009     C   50              EVAL      VABCTM = '7Q'
047100000801     C*
047200000801     C                   ENDSR
047300091223     C*----------------------------------------------------*
047400091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
047500091223     C*----------------------------------------------------*
047600091223     C     REPNSP        BEGSR
047700091223     C*
047800091223     C                   EXSR      INZVAR
047900091223     C                   EXSR      DEFCAM
048000091223     C*
048100091223     C* NSP => Stacco un numeratore da AZNUM
048200091223     C                   clear                   TRUL33DS
048300091223     C                   eval      I33OPE = *zeros
048400091223     C                   eval      I33CNU = 302
048500091223     C                   eval      I33NUM = 1
048600091223     C                   movel     TRUL33DS      KPJBU
048700091223     C                   call      'TRUL33R'
048800091223     C                   parm                    KPJBA
048900091223     C                   movel     KPJBU         TRUL33DS
049000091223     C                   if        O33ERR = *zeros
049100091223     C                   z-add     O33NRF        VABNSP
049200091223     C                   z-add     O33NRF        VATNSP
049300091223     C                   else
049400091223     C                   SETON                                        31
049500091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049600091223     C                             + ' ' + 'VABNSP VATNSP'
049700091223     C                   endif
049800091223     C*
049900091223     C                   ENDSR
050000000801     C*----------------------------------------------------*
050100021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
050200000801     C*----------------------------------------------------*
050300010201     C     IMPVAB        BEGSR
050400070530     C*
050500070530     C                   SETOFF                                       3132
050600070928     C*
050700070928     C                   MOVE(P)   vlrpoi        VABFGS
050800070928     C                   MOVE(P)   vlrpoi        VATFGS
050900070928     C*
051000070928     C                   MOVEL     datcor        VABAAS
051100070928     C                   MOVEL     datcor        VATAAS
051200070928     C                   MOVE      datcor        VABMGS
051300100607     C                   EVAL      VABRSD=%trim(%subst(vindta:34:40))
051400100503     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
051500100503     C     '@':'A'       XLATE     VABRSD        VABRSD
051600100503     C* ==
051700100607     C                   EVAL      VABRD2=%trim(%subst(vindta:74:40))
051800100607     C                   EVAL      VABIND=%trim(%subst(vindta:114:40))+
051900100607     C                                    %trim(%subst(vindta:154:40))+
052000100607     C                                    %trim(%subst(vindta:194:40))
052100100607     C                   EVAL      VABLOD=%trim(%subst(vindta:244:40))
052200100607     C***                EVAL      VABPRD=%trim(%subst(vindta:314:5))
052300100607     C                   EVAL      VABRMA=%trim(%subst(vindta:16:8))
052400100607     C                   MOVEL     *blanks       wNOTE            70
052500100607     C                   EVAL      wNOTE =%trim(%subst(vindta:384:70))+
052600100607     C                                    %trim(%subst(vindta:454:70))
052700100607     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
052800100607     C                   EVAL      VABNT2=%trim(%subst(wNOTE:36:35))
052900100607     C*
053000100607     C* NZD
053100100607     C                   EVAL      wNAZ=%trim(%subst(vindta:283:45))
053200100607     C* ...passo dal codice ISO a quello "Bartolini"
053300100607     C                   Z-ADD     1             jNAZ
053400100607     C     wNAZ          LOOKUP    skNAZISO(jNAZ)                         55
053500100607     C                   IF        %found
053600100607     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
053700100607     C                   ENDIF
053800100923     C*
053900100923     C* CCM
054000100923     C                   IF        %subst(vindta:524:23) <> *blanks
054100100923     C                   EVAL      PiStr=%trim(%subst(vindta:524:23))
054200100923     C                   EXSR      CHKNUM
054300100923     C                   IF        PiInt=*on
054400100923     C                   Z-ADD     PiVal         VABCCM
054500100923     C                   Z-ADD     PiVal         VATCCM
054600100923     C                   ELSE
054700100923     C                   SETON                                        31
054800100923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054900100923     C                             + ' ' + 'VABCCM VATCCM'
055000100923     C                   ENDIF
055100100923     C                   ENDIF
055200100520     C*
055300100525     C* NSP
055400100525     C                   IF        *in50 = *off
055500100607     C                   EVAL      PiStr=%trim(%subst(vindta:16:8))
055600100525     C                   EXSR      CHKNUM
055700100525     C                   IF        PiInt=*on
055800100525     C                   Z-ADD     PiVal         VABNSP
055900100525     C                   Z-ADD     PiVal         VATNSP
056000100525     C                   ELSE
056100100525     C                   SETON                                        31
056200100525     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056300100525     C                             + ' ' + 'VABNSP VATNSP'
056400100525     C                   ENDIF
056500100525     C                   ENDIF
056600100525     C* RMN
056700100607     C                   EVAL      PiStr=%trim(%subst(vindta:16:8))
056800100607     C                   EXSR      CHKNUM
056900100607     C                   IF        PiInt=*on
057000100607     C                   Z-ADD     PiVal         VABRMN
057100100607     C                   ELSE
057200100607     C                   SETON                                        32
057300100607     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
057400100607     C                             + ' ' + 'VABRMN'
057500100607     C                   ENDIF
057600100503     C* CAD
057700100607     C                   EVAL      PiStr=%trim(%subst(vindta:234:10))
057800100607     C                   EXSR      CHKNUM
057900100607     C                   IF        PiInt=*on
058000100607     C                   Z-ADD     PiVal         Num5_0
058100100607     C                   MOVEL     Num5_0        VABCAD
058200100607     C                   ELSE
058300100607     C                   SETON                                        32
058400100607     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
058500100607     C                             + ' ' + 'VABCAD'
058600100607     C                   ENDIF
058700100503     C* NCL
058800100607     C                   EVAL      PiStr=%trim(%subst(vindta:327:5))
058900100607     C                   EXSR      CHKNUM
059000100607     C                   IF        PiInt=*on
059100100607     C                   Z-ADD     PiVal         VABNCL
059200100607     C                   ELSE
059300100607     C                   SETON                                        32
059400100607     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
059500100607     C                             + ' ' + 'VABNCL'
059600100607     C                   ENDIF
059700100503     C* PKB
059800100607     C                   EVAL      PiStr=%trim(%subst(vindta:332:7))
059900100607     C                   EXSR      CHKNUM
060000100607     C                   IF        PiNum=*on
060100100607     C                   EVAL      PiVal=PiVal / 100                            * gestisco 2 dec.
060200100607     C                   Z-ADD     PiVal         VABPKB
060300100607     C                   ELSE
060400100607     C                   SETON                                        32
060500100607     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
060600100607     C                             + ' ' + 'VABPKB'
060700100607     C                   ENDIF
060800100607     C* VLB
060900100607     C                   EVAL      PiStr=%trim(%subst(vindta:339:7))
061000100607     C                   EXSR      CHKNUM
061100100607     C                   IF        PiNum=*on
061200100607     C                   EVAL      PiVal=PiVal / 100                            * gestisco 2 dec.
061300100607     C                   Z-ADD     PiVal         VABVLB
061400100607     C                   ELSE
061500100607     C                   SETON                                        32
061600100607     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
061700100607     C                             + ' ' + 'VABVLB'
061800100607     C                   ENDIF
061900100503     C* CAS
062000100607     C                   IF        %trim(%subst(vindta:346:9))<>*blanks
062100100607     C                             AND
062200100607     C                             %trim(%subst(vindta:346:9))<>*zeros
062300100607     C                   MOVEL     '1'           FlgCAS
062400100607     C                   EVAL      PiStr=%trim(%subst(vindta:346:9))
062500100607     C                   EXSR      CHKNUM
062600100607     C                   IF        PiNum=*on
062700100607     C                   EVAL      PiVal=PiVal / 100                            * gestisco 2 dec.
062800100607     C                   Z-ADD     PiVal         VABCAS
062900100607     C                   ELSE
063000100607     C                   SETON                                        32
063100100607     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
063200100607     C                             + ' ' + 'VABCAS'
063300100607     C                   ENDIF
063400100607     C                   ENDIF
063500100310     C*
063600100310     C* Se provincia nn valorizzata la reperisco
063700100310     C* tramite TISI95R a seconda dei dati d instradamento presenti
063800100607     C                   IF        VABNZD  = *blanks AND
063900100607     C                             VABPRD  = *blanks AND
064000100607     C                             VABCAD <> *blanks
064100100607     C                   CLEAR                   TISI95DS
064200100607     C                   EVAL      I95TCN = '3'
064300100607     C                   Z-ADD     datcor        I95DAT
064400100607     C                   EVAL      I95NAR = VABNZD
064500100607     C                   EVAL      I95CAP = VABCAD
064600100607     C                   EVAL      I95LOC = VABLOD
064700100607     C                   CALL      'TISI95R'
064800100607     C                   PARM                    TISI95DS
064900100607     C                   EVAL      VABPRD = O95PRV
065000100607     C                   ENDIF
065100070730     C*
065200070730     C* Considerazioni finali su CBO/CAS
065300100525     C***                IF        FlgCAS = '1'
065400100525     C***                IF        VABCBO = '1'
065500100525     C***                EVAL      VABCBO = '4'
065600100525     C***                ENDIF
065700100525     C***                IF        VABCBO = '2'
065800100525     C***                EVAL      VABCBO = '6'
065900100525     C***                ENDIF
066000100525     C***                ENDIF
066100010202     C*
066200000801     C* Ebbene...
066300000801     C                   ADD       1             §CTRMO
066400070530     C                   IF        *in31 <> *zeros OR
066500070530     C                             *in32 <> *zeros
066600000801     C                   ADD       1             §CTRNO
066700000801     C                   EVAL      recko = vindta
066800000801     C                   EXCEPT    rigadett
066900020725     C                   EVAL      x_vinflg = '2'
067000000801     C                   ELSE
067100010201     C                   ADD       1             §CTROKVB
067200000801     C                   ENDIF
067300000801     C*
067400000801     C                   ENDSR
067500100127     C*----------------------------------------------------*
067600100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
067700100127     C*----------------------------------------------------*
067800100127     C     WRIVAT_A      BEGSR
067900100127     C*
068000100127     C* Valorizzo l buffer di scrittura del FIVAT
068100100127     C* - TIPO RECORD "A"
068200100412     C***                eval      VATTRC = 'A'
068300100412     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
068400100412     C***                exsr      wrivat
068500100127     C*
068600100127     C                   ENDSR
068700010201     C*----------------------------------------------------*
068800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
068900010201     C*----------------------------------------------------*
069000071003     C     WRIVAT_B      BEGSR
069100010201     C*
069200021113     C* Valorizzo l buffer di scrittura del FIVAT
069300070928     C* - TIPO RECORD "B"
069400100412     C***                eval      VATTRC = 'B'
069500100412     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
069600100412     C***                exsr      wrivat
069700010201     C*
069800010201     C                   ENDSR
069900071003     C*----------------------------------------------------*
070000071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
070100071003     C*----------------------------------------------------*
070200071003     C     WRIVAT_E      BEGSR
070300071003     C*
070400071003     C* Valorizzo l buffer di scrittura del FIVAT
070500071003     C* - TIPO RECORD "E"
070600100607     C                   eval      VATTRC = 'E'
070700100607     C                   eval      InStringa = %trim(%subst(vindta:547:1000))
070800100607     C*
070900100607     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
071000100607     C                   CALL      'XSPLIT2'
071100100607     C                   PARM                    InStringa
071200100607     C                   PARM      ';'           InSepara
071300100607     C                   PARM      '6'           InTypeOut
071400110616     C                   PARM      ''            wSkParam
071500100607     C                   PARM                    OutErrore
071600100607     C                   PARM                    OutMsg
071700100607     C                   MOVEA     wSkParam      SkSplitCSV_6
071800100607     C*
071900100607     C* Valorizzo la schiera delle intestazioni colonne (elementi)
072000100607     C                   z-add     1             i                 4 0
072100100607     C                   dow       i <= %elem(SkSplitCSV_6)
072200100607     C                   if        SkSplitCSV_6(i) = *blanks
072300100607     C                   leave
072400100607     C                   else
072500100607     C                   eval      vatnot = SkSplitCSV_6(i)
072600100607     C                   exsr      wrivat
072700100607     C                   endif
072800100607     C                   add       1             i
072900100607     C                   enddo
073000071003     C*
073100071003     C                   ENDSR
073200100127     C*----------------------------------------------------*
073300100127     C*  SCARICO BUFFER FIVAT
073400100127     C*----------------------------------------------------*
073500100127     C     WRIVAT        BEGSR
073600100127     C*
073700100127     C                   if        vatNOT <> *blanks
073800100127     C                   ADD       1             §CTROKVT
073900100127     C                   write     FIVAT000
074000100127     C                   endif
074100100127     C*
074200100127     C                   ENDSR
074300020904
074400020904
074500010202     C*----------------------------------------------------*
074600100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
074700010202     C*----------------------------------------------------*
074800100525     C     PREVASX       BEGSR
074900100525     C*
075000021113     C* Compongo il nome del membro da dare al FIVATWWR
075100010202     C                   eval      parmbr = vlrhdl
075200010202     C                   movel     'M'           parmbr
075300070530     C                   eval      parccm = %subst(vlrKSC:2:7)
075400010202     C                   eval      paropz = '1'
075500100525     C*
075600010202     C* Effettuo la chiamata al CLLE preposto
075700100525     C  N51              call(e)   'TITVVTC'
075800010202     C                   parm                    parccm
075900010202     C                   parm                    parmbr
076000010202     C                   parm                    paropz
076100100525     C   51              call(e)   'TITVVBC'
076200100525     C                   parm                    parccm
076300100525     C                   parm                    parmbr
076400100525     C                   parm                    paropz
076500100525     C*
076600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
076700010202     C                   if        %error
076800010202     C                   movel     '1'           chkcall
076900010202     C                   else
077000010202     C                   movel     '0'           chkcall
077100010202     C                   endif
077200010202     C*
077300010202     C                   ENDSR
077400000801     C*----------------------------------------------------*
077500000801     C*  CONTROLLO NUMERICITA' CAMPI
077600000801     C*----------------------------------------------------*
077700000801     C     CHKNUM        BEGSR
077800081113     C*
077900081113     C                   IF        PiDecChr = *blanks
078000100503     C                   EVAL      PiDecChr = '.'
078100081113     C                   ENDIF
078200091223     C*
078300081113     C                   callp(e)  UBISNUM_Check(PiStr
078400081113     C                                          :PiDecChr
078500081113     C                                          :PiVal
078600081113     C                                          :PiNum
078700081113     C                                          :PiInt)
078800081113     C*
078900000801     C                   IF        %error
079000000801     C                   EVAL      PiInt=*off
079100000801     C                   ENDIF
079200000801     C*
079300000801     C                   ENDSR
079400000801     C***
079500100607
079600100607
079700100607     C*--------------------------------------------------------
079800100607     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
079900100607     C*--------------------------------------------------------
080000100607     C     CARTAB        BEGSR
080100100607     C*
080200100607     C* TABELLA '15' - NAZIONI
080300100607     C                   clear                   skNAZISO
080400100607     C                   clear                   skNAZBAR
080500100607     C                   eval      tblKUT = 1
080600100607     C                   eval      tblCOD = '15'
080700100607     C     KEYtabP       setll     tabel00f
080800100607     C     KEYtabP       reade     tabel00f
080900100607     C                   dow       not %eof(tabel00f)
081000100607     C                   if        tblFLG = *blanks
081100100607     C                   movel(p)  tblUNI        ds15
081200100607     C                   add       1             jNAZ
081300100607     C                   eval      skNAZBAR(jNAZ) = tblKEY
081400100607     C                   eval      skNAZISO(jNAZ) = §15COD
081500100607     C                   endif
081600100607     C     KEYtabP       reade     tabel00f
081700100607     C                   enddo
081800100607     C*
081900100607     C                   ENDSR
082000100607     C***
082100011113
082200011113
082300000801
082400000801
082500990920      /TITLE Invio dei dati al punto operativo.
082600010202     C     invio         BEGSR
082700990920     C*
082800021113     C* 1° invio FIVAT
082900010201     C                   reset                   dscmz
083000010201     C                   move      vlrpoi        cmzdst
083100021113     C                   eval      cmzfld = 'FIVATWWR'
083200010201     C                   eval      cmzmbd = vlrhdl
083300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
083400021009     C***                if        prmfir = *blanks
083500021113     C                   eval      cmzfla = 'FIVAT00F'
083600021113     C                   eval      cmzmba = 'FIVAT00F'
083700021009     C***                else
083800021009     C***                eval      cmzfla = prmfir
083900021009     C***                eval      cmzmba = prmfir
084000021009     C***                endif
084100010201     C                   eval      cmznrr = *zeros
084200020305     C                   move      §ctrokvt      cmznrr
084300021018     C                   eval      cmzlba = vlrfl1
084400010201     C                   call(e)   'TIS711C'
084500010201     C                   parm                    dscmz
084600010201     C                   parm      *blanks       esito
084700010205     C                   if        %error
084800010205     C                             or cmzerr = '1'
084900010205     C                             or esito  = '1'
085000010205     C                   eval      wrkesito = '3'
085100010205     C                   else
085200010201     C*
085300021113     C* 2° invio FIVAB
085400100525     C                   if        *in51 = *off
085500010201     C                   reset                   dscmz
085600010201     C                   move      vlrpoi        cmzdst
085700010201     C                   eval      cmzfld = vlrfou
085800010201     C                   eval      cmzmbd = vlrhdl
085900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
086000021009     C***                if        prmfir = *blanks
086100021113     C                   eval      cmzfla = 'FIVAB00F'
086200021113     C                   eval      cmzmba = 'FIVAB00F'
086300021009     C***                else
086400021009     C***                eval      cmzfla = prmfir
086500021009     C***                eval      cmzmba = prmfir
086600021009     C***                endif
086700010201     C                   eval      cmznrr = *zeros
086800010201     C                   move      §ctrokvb      cmznrr
086900021018     C                   eval      cmzlba = vlrfl1
087000010201     C                   call(e)   'TIS711C'
087100010201     C                   parm                    dscmz
087200010201     C                   parm      *blanks       esito
087300010201     C                   if        %error
087400010201     C                             or cmzerr = '1'
087500010201     C                             or esito  = '1'
087600010201     C                   eval      wrkesito = '3'
087700010201     C                   endif
087800100525     C                   endif
087900010205     C                   endif
088000990920     C*
088100000613     C                   ENDSR
088200000613     C***
088300070411
088400070411     C     *pssr         BEGSR
088500070411     C*
088600070411     C                   if        %open(tivin00r)
088700070411     C                   close     tivin00r
088800070411     C                   endif
088900070411     C                   if        %open(fivabwwr)
089000070411     C                   close     fivabwwr
089100070411     C                   endif
089200070411     C                   if        %open(fivatwwr)
089300070411     C                   close     fivatwwr
089400070411     C                   endif
089500070411     C*
089600070411     C* Effettuo la chiamata al CLLE preposto
089700100525     C  N51              call(e)   'TITVVTC'
089800100525     C                   parm                    parccm
089900100525     C                   parm                    parmbr
090000100525     C                   parm      '2'           paropz
090100100525     C   51              call(e)   'TITVVBC'
090200100525     C                   parm                    parccm
090300100525     C                   parm                    parmbr
090400100525     C                   parm      '2'           paropz
090500070411     C*
090600070411     C                   eval      wrkesito = '2'
090700070411     C*
090800070411     C                   seton                                        LR
090900070411     C*
091000070411     C                   ENDSR     '*CANCL'
091100070411     C***
091200070411
091300990910
091400000613     C     *inzsr        BEGSR
091500990910     C*
091600990910     C     *entry        plist
091700990920     C                   parm                    tivlrds
091800990921     C                   parm      wrkesito      esito
091900000724     C                   parm                    prmlit
092000000710     C                   parm                    prmfir
092100100607     C*
092200100607     C* Chiave su TABEL00F - parziale
092300100607     C     KEYtabP       KLIST
092400100607     C                   KFLD                    tblKUT
092500100607     C                   KFLD                    tblCOD
092600000613     C*
092700000830     C* CALCOLA LA DATA CORRENTE
092800091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
092900091223     C                   eval      datcor = %dec(%date() : *ISO)
093000000830     C*
093100000613     C                   ENDSR
093200000613     C***
093300990908
093400100607     Otitv1y2p  E            riepilogo         2
093500990915     O                                              'Upload via Internet'
093600990915     O                                           +1 'Traduzione TIVIN00R -'
093700021113     O                                           +1 'FIVABWWR/FIVATWWR'
093800010201     O                                           +1 'Report testate bolle'
093900990915     O          E            riepilogo   2
094000990915     O                       wrkdata
094100990915     O                       wrkora              +1
094200990915     O                       procname            +1
094300990915     O          E            riepilogo   2
094400990915     O                                              'Cliente..................:'
094500990915     O                       VABCCM        z     +1
094600990915     O          E            riepilogo   2
094700990920     O                                              'Riferimento Strategi.....:'
094800990920     O                       vlrhdl              +1
094900990915     O          E            riepilogo   2
095000990915     O                                              'Giusti...................:'
095100010201     O                       §CTROKVB      2   +  1
095200100525     O                       §CTROKVT      2   +  1
095300990915     O          E            riepilogo   2
095400990915     O                                              'Sbagliati e corretti.....:'
095500971022     O                       §CTRMO        2   +  1
095600990915     O          E            riepilogo   2
095700990915     O                                              'Sbagliati e scartati.....:'
095800971022     O                       §CTRNO        2   +  1
095900000613
096000100607     Otitv1y2ps E            testdett          2
096100000613     O                                              'Upload via Internet'
096200000613     O                                           +1 'Traduzione TIVIN00R -'
096300021113     O                                           +1 'FIVABWWR/FIVATWWR'
096400010201     O                                           +1 'Report testate bolle'
096500000616     O          E            testdett    3
096600000613     O                                           +2 'N° rec'
096700000613     O                                           +3 'Anteprima contenuto'
096800000616     O          E            rigadett    2
096900000613     O                       rrnum               +2
097000000621     O                       recko               +3
097100000616     O          E            findett     2
097200000613     O                       wrkdata
097300000613     O                       wrkora              +1
097400000613     O                       procname            +1
097500000616     O          E            findett     2
097600000613     O                                              'Cliente..................:'
097700000613     O                       VABCCM        z     +1
097800000616     O          E            findett     2
097900000613     O                                              'Riferimento Strategi.....:'
098000000613     O                       vlrhdl              +1
098100000616     O          E            findett     2
098200000613     O                                              'Giusti...................:'
098300010201     O                       §CTROKVB      2   +  1
098400100525     O                       §CTROKVT      2   +  1
098500000616     O          E            findett     2
098600000613     O                                              'Sbagliati e corretti.....:'
098700000613     O                       §CTRMO        2   +  1
098800000616     O          E            findett     2
098900000613     O                                              'Sbagliati e scartati.....:'
099000000613     O                       §CTRNO        2   +  1
099100000512** CMD - COMANDI CL
099200100607OVRPRTF FILE(TITV1Y2P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
099300100607OVRPRTF FILE(TITV1Y2PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
099400100607DLTOVR FILE(TITV1Y2P TITV1Y2PS) LVL(*)
099500000512
099600000512
