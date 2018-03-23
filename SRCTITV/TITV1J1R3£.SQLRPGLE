000100100713      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200130114     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600100713     FEDIVABwr  O    E             DISK    usropn
000700100713     FEDIVATwr  O    E             DISK    usropn
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
003600100713     D depspe          s             10    INZ(*blanks)
003700100713     D curspe          s             10    INZ(*blanks)
003800010202     D parccm          s              8    INZ(*blanks)
003900010202     D parmbr          s             10    INZ(*blanks)
004000010202     D paropz          s              1    INZ(*blanks)
004100010202     D chkcall         s              1    INZ(*blanks)
004200080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
004300091223     D Num5_0          s              5  0
004400100713     D wNomeFile       s             30    INZ(*blanks)
004500100713
004600100713     D*------------------
004700100713     D* Costanti
004800100713     D*------------------
004900100713     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005000100713     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005100020725
005200020725     D*------------------
005300020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
005400020725     D*------------------
005500070530     D INPUT_DS        DS                  INZ
005600020725     D  VINDTA                 1   2048
005700020725     D  VINFLG              2049   2049
005800020725     D  VINSPE              2050   2059
005900020725     D  VINRRN              2060   2067  0
006000020725     D*
006100091223
006200081113
006300081113     D*------------------
006400081113     D* LINKING A DEFINIZIONI ESTERNE
006500081113     D*------------------
006600081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006700090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006800081113
006900990908
007000010201
007100010201
007200080117     C*
007300080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007400080117     C
007500080117     C/EXEC SQL
007600080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007700080117     C/END-EXEC
007800080117     C
007900990915     C                   time                    wrkdata
008000990915     C                   time                    wrkora
008100000913     C                   reset                   rrnum
008200990921     C                   reset                   esito
008300990921     C                   reset                   wrkesito
008400000724     C*
008500000724     C* SE OCCORRE SPEDIRE IN FILIALE
008600000724     C                   if        vlrpoi <> *zeros
008700000724     C*
008800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009000000724     C     vlrpoi        chain     azorg01l
009100000724     C                   if        %found
009200000616     C                   movel(p)  CMD(1)        depcmd
009300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009400000616     C*
009500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009600011113     C                   Z-ADD     150           LENGH            15 5
009700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009800000616     C                   PARM                    depcmd
009900000616     C                   PARM                    LENGH
010000000724     C*
010100000724     C                   endif
010200000724     C                   endif
010300000616     C*
010400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010500000616     C                   movel(p)  CMD(2)        depcmd
010600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010700000616     C*
010800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010900011113     C                   Z-ADD     150           LENGH            15 5
011000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011100000616     C                   PARM                    depcmd
011200000616     C                   PARM                    LENGH
011300000616     C*
011400100629     C                   if        not %open(titv1y4ps)
011500100629     C                   open      titv1y4ps
011600130114     C***                except    testdett
011700000613     C                   endif
011800000613     C*
011900100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
012000070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
012100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012200000613     C*
012300010202     C* Effettuo la chiamata al CLLE preposto
012400100714     C  N51              call(e)   'TITVEVTC'
012500100525     C                   parm                    parccm
012600100525     C                   parm                    parmbr
012700100525     C                   parm      '2'           paropz
012800100714     C   51              call(e)   'TITVEVBC'
012900100525     C                   parm                    parccm
013000100525     C                   parm                    parmbr
013100100525     C                   parm      '2'           paropz
013200070730     C*
013300070730     C* Effettuo lancio TISI95 solo x chiusura
013400070730     C                   CLEAR                   TISI95DS
013500070730     C                   EVAL      I95TLA = 'C'
013600070730     C                   CALL      'TISI95R'
013700070730     C                   PARM                    TISI95DS
013800010202     C*
013900100629     C                   if        %open(titv1y4ps)
014000130114     C***                except    findett
014100100629     C                   close     titv1y4ps
014200000613     C                   endif
014300000616     C*
014400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
014500011113     C                   Z-ADD     150           LENGH            15 5
014600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014700000616     C                   PARM                    CMD(3)
014800000616     C                   PARM                    LENGH
014900000616     C*
015000000801     C
015100010201     C                   seton                                        LR
015200000613
015300100310
015400100310
015500000613     C*--------------------------------------------------------
015600000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015700000613     C*--------------------------------------------------------
015800000621     C     STPR          BEGSR
015900000613     C*
016000100629     C                   if        not %open(titv1y4p)
016100100629     C                   open      titv1y4p
016200990915     C                   endif
016300990915     C*
016400130114     C***                except    riepilogo
016500990915     C*
016600100629     C                   if        %open(titv1y4p)
016700100629     C                   close     titv1y4p
016800990914     C                   endif
016900990910     C*
017000000613     C                   ENDSR
017100000613     C***
017200990908
017300000801
017400910830     C*--------------------------------------------------------
017500100713     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
017600910830     C*--------------------------------------------------------
017700070530     C     RWFILE        BEGSR
017800990910     C*
017900990914     C                   if        not %open(tivin00r)
018000990908     C                   open      tivin00r
018100990914     C                   endif
018200070530     C*
018300100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
018400100525     C                   exsr      prevasx
018500010201     C*
018600010202     C                   if        chkcall = '0'
018700010202     C*
018800100713     C                   if        not %open(edivabwr)
018900100713     C                   open      edivabwr
019000100525     C                   endif
019100100525     C*
019200100713     C                   if        not %open(edivatwr)
019300100713     C                   open      edivatwr
019400010201     C                   endif
019500080117     C*
019600100714     C                   exsr      inzvar
019700100714     C                   exsr      defcam
019800990910     C*
019900010201     C                   clear                   §CTROKVB          5 0
020000020305     C                   clear                   §CTROKVT          5 0
020100000801     C                   clear                   §CTRMO            5 0
020200000801     C                   clear                   §CTRNO            5 0
020300990910     C*
020400020725     C/EXEC SQL
020500020725     C+ declare C1 cursor for select
020600100713     C+ vindta, vinflg, substr(vindta, 20, 10) as sped, rrn(tivin00r)
020700060223     C+ from tivin00r
020800060223     C+ order by sped
020900020725     C+ for read only
021000020725     C/END-EXEC
021100020725     C
021200020725     C/EXEC SQL
021300020725     C+ open C1
021400020725     C/END-EXEC
021500020725     C
021600020725     C/EXEC SQL
021700070530     C+ Fetch C1 into :INPUT_DS
021800020725     C/END-EXEC
021900020725     C*
022000020725     C                   dow       sqlcod = *zeros
022100990913     C*
022200100310     C                   if        vindta > *blanks
022300000613     C                   add       1             rrnum
022400000801     C*
022500020725     C                   if        vinflg = *blanks
022600020725     C                             or vinflg = '0'
022700020725     C                             or vinflg = '2'
022800000801     C*
022900020725     C                   clear                   x_vinmsg
023000020725     C                   eval      x_vinflg = '1'
023100010305     C*
023200010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
023300100713     C                   EVAL      curspe=%trim(%subst(vindta:20:10))
023400010305     C*
023500100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
023600080923     C                   if        *in50 = *off
023700071003     C                   exsr      impvab
023800100525     C                   exsr      wrivab
023900100127     C                   exsr      wrivat_a                                     => carico VAT
024000071003     C                   exsr      wrivat_b                                     => carico VAT
024100100525     C   51              exsr      wrivat_e                                     => carico VAT
024200071003     C                   else
024300080923     C*
024400071009     C                   if        wDISK = 'D'
024500100714     C                   exsr      inzvar
024600100714     C                   exsr      defcam
024700071009     C                   exsr      impvab
024800100412     C                   exsr      wrivab
024900100127     C                   exsr      wrivat_a                                     => carico VAT
025000071009     C                   exsr      wrivat_b                                     => carico VAT
025100071009     C                   exsr      wrivat_e                                     => carico VAT
025200071009     C                   else
025300071009     C*
025400010305     C                   if        depspe = *blanks                             => 1° giro
025500010305     C                   eval      depspe = curspe                              => memorizz. spediz
025600080117     C                   clear                   tivinds
025700100714     C                   exsr      inzvar
025800100714     C                   exsr      defcam
025900020305     C                   exsr      impvab
026000100127     C                   exsr      wrivat_a                                     => carico VAT
026100071003     C                   exsr      wrivat_b                                     => carico VAT
026200071003     C   50              exsr      wrivat_e                                     => carico VAT
026300010305     C                   else
026400020725     C                   if        depspe <> curspe                             => rottura di spediz
026500010305     C                   eval      depspe = curspe                              => memorizz. spediz
026600100412     C                   exsr      wrivab
026700080117     C                   clear                   tivinds
026800100714     C                   exsr      inzvar
026900100714     C                   exsr      defcam
027000020305     C                   exsr      impvab
027100100127     C                   exsr      wrivat_a                                     => carico VAT
027200071003     C                   exsr      wrivat_b                                     => carico VAT
027300071003     C   50              exsr      wrivat_e                                     => carico VAT
027400020305     C                   else                                                   => x stessa spediz
027500100401     C*
027600100401     C* Se nn richiesta esclusione spedizioni "duplicate"
027700100412     C                   if        wDUPREC <> 'N'
027800020305     C                   exsr      impvab
027900100127     C                   exsr      wrivat_a                                     => carico VAT
028000071003     C                   exsr      wrivat_b                                     => carico VAT
028100071003     C   50              exsr      wrivat_e                                     => carico VAT
028200100401     C                   endif
028300010305     C                   endif
028400010305     C                   endif
028500010305     C                   endif
028600071003     C                   endif
028700071009     C                   endif
028800000905     C*
028900000905     C                   else
029000020725     C                   eval      x_vinflg = '1'
029100000905     C                   endif
029200000905     C*
029300020725     C     VINRRN        chain     tivin000
029400020725     C                   if        %found(tivin00r)
029500020725     C                   eval      y_vinflg = x_vinflg
029600020725     C                   eval      y_vinmsg = x_vinmsg
029700020725     C                   update    tivin000
029800020725     C                   endif
029900020725     C*
030000020725     C/EXEC SQL
030100070530     C+ Fetch C1 into :INPUT_DS
030200020725     C/END-EXEC
030300020725     C*
030400020725     C                   enddo
030500020725     C*
030600020725     C/EXEC SQL
030700020725     C+ close C1
030800020725     C/END-EXEC
030900000905     C*
031000020305     C* Scarico i VAB rimasti "in sospeso"
031100071009     C                   if        wDISK = 'C'
031200100412     C                   exsr      wrivab
031300071009     C                   endif
031400010202     C*
031500010202     C                   endif
031600990910
031700990910     C* Se non ci sono record con errori ...
031800000710     C                   if        §ctrno = 0
031900990910     C* ... restituisco esito OK.
032000990921     C                   eval      wrkesito = '0'
032100990910     C                   else
032200100525     C                   if        §ctrokvb > 0 OR
032300100525     C                             §ctrokvt > 0
032400990921     C                   eval      wrkesito = '1'
032500000710     C                   else
032600000710     C                   eval      wrkesito = '2'
032700990910     C                   endif
032800000710     C                   endif
032900990910     C*
033000990914     C                   if        %open(tivin00r)
033100990908     C                   close     tivin00r
033200990914     C                   endif
033300100713     C                   if        %open(edivabwr)
033400100713     C                   close     edivabwr
033500990914     C                   endif
033600100713     C                   if        %open(edivatwr)
033700100713     C                   close     edivatwr
033800010201     C                   endif
033900990910     C*
034000100525     C                   if        §ctrokvb > 0 OR
034100100525     C                             §ctrokvt > 0
034200000724     C                             and vlrpoi <> *zeros
034300010202     C                   exsr      invio
034400990920     C                   endif
034500990920     C*
034600910830     C                   ENDSR
034700000613     C***
034800100412
034900100412
035000010305
035100010305     C*----------------------------------------------------*
035200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
035300010305     C*----------------------------------------------------*
035400020305     C     WRIVAB        BEGSR
035500100713     C*
035600100713     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
035700100714     C                   EVAL      VABCMR = %char(VABCCM)
035800100713     C                   EVAL      VABDCM = DATCOR
035900100713     C                   EVAL      VABDTS = DATCOR
036000100713     C                   EVAL      VABHMS = ORACOR
036100100713     C                   EVAL      VABCNT = 1
036200100412     C*
036300100412     C* Gestisco l'eventuale rottura x numero spedizione
036400100412     C                   if        VABRMA = *blanks
036500100412     C                   movel(P)  VABRMN        VABRMA
036600100412     C                   endif
036700100412     C*
036800100525     C                   if        *in51 = *off and *in31 = *off
036900100713     C                   write     edivab00                                     => scarico il VAB
037000100525     C                   endif
037100010305     C*
037200010305     C                   ENDSR
037300990920
037400000801     C*----------------------------------------------------*
037500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
037600000801     C*----------------------------------------------------*
037700010201     C     INZVAR        BEGSR
037800000801     C*
037900010201     C                   Z-ADD     *zeros        Num5_0
038000020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
038100020725     C                   MOVEL     '0'           FlgCAS            1
038200100713     C                   MOVEL     *blanks       wNOTE            70
038300000801     C*
038400000801     C                   ENDSR
038500000801     C*----------------------------------------------------*
038600000801     C*  IMPOSTAZIONE CAMPI COSTANTI
038700000801     C*----------------------------------------------------*
038800020904     C     DEFCAM        BEGSR
038900080923     C*
039000100525     C                   SETOFF                                       5051
039100000801     C*
039200100713     C                   CLEAR                   EDIVAB00
039300100713     C                   CLEAR                   EDIVAT00
039400100713     C* Imposto i valori di default...
039500100713     C                   EVAL      VABCCM = 2220929
039600100713     C                   EVAL      VABLNP = 222
039700100713     C                   EVAL      VABCTR = 001
039800100713     C                   EVAL      VABTSP = 'C'
039900100713     C                   EVAL      VABCBO = '1'
040000070222     C* ... e poi verifico se sono stati passati come parametri
040100070222     C                   IF        vlrppt > *blanks
040200100525     C*
040300070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
040400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
040500070222     C                   EXSR      CHKNUM
040600070222     C                   IF        PiInt=*on
040700070222     C                   Z-ADD     PiVal         VABCCM
040800070222     C                   Z-ADD     PiVal         VATCCM
040900070222     C                   ENDIF
041000070222     C                   ENDIF
041100070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
041200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
041300070222     C                   EXSR      CHKNUM
041400070222     C                   IF        PiInt=*on
041500070222     C                   Z-ADD     PiVal         VABLNP
041600070222     C                   Z-ADD     PiVal         VATLNP
041700070222     C                   ENDIF
041800070222     C                   ENDIF
041900070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
042000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
042100070222     C                   EXSR      CHKNUM
042200070222     C                   IF        PiInt=*on
042300070222     C                   Z-ADD     PiVal         VABCTR
042400070222     C                   ENDIF
042500070928     C                   ENDIF
042600071009     C                   MOVEL     *blanks       wDISK             1
042700071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
042800071009     C                   IF        wDISK <> *blanks
042900070928     C                   SETON                                        50
043000070222     C                   ENDIF
043100100525     C                   IF        wDISK  = 'T'
043200100525     C                   SETOFF                                       50
043300100525     C                   SETON                                        51
043400100525     C                   ENDIF
043500100401     C                   MOVEL     *blanks       wDUPREC           1
043600100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
043700100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
043800100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
043900100525     C                   ENDIF
044000100525     C*
044100070222     C                   ENDIF
044200071009     C*
044300071009     C   50              EVAL      VABCTM = '7Q'
044400000801     C*
044500000801     C                   ENDSR
044600000801     C*----------------------------------------------------*
044700100713     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
044800000801     C*----------------------------------------------------*
044900010201     C     IMPVAB        BEGSR
045000070530     C*
045100070530     C                   SETOFF                                       3132
045200070928     C*
045300070928     C                   MOVE(P)   vlrpoi        VABFGS
045400070928     C*
045500070928     C                   MOVEL     datcor        VABAAS
045600070928     C                   MOVE      datcor        VABMGS
045700100713     C*
045800100713     C* Porto tutto il buffer d input in maiuscolo
045900100713     C     minu:maiu     XLATE     vindta        vindta
046000100713     C*
046100100713     C* Reperimento campi ALFA
046200100713     C                   EVAL      VABRSD=%trim(%subst(vindta:74:35))
046300100714     C                   EVAL      VABIND=%trim(%subst(vindta:109:40))+' '+
046400100715     C                                    %trim(%subst(vindta:624:40))
046500100713     C                   EVAL      VABLOD=%trim(%subst(vindta:149:40))
046600100713     C                   EVAL      VABPRD=%trim(%subst(vindta:199:2))
046700100713     C***                EVAL      VABRMA=%trim(%subst(vindta:20:10))
046800100713     C                   EVAL      VABRMA=%trim(%subst(vindta:38:10))
046900100713     C                   IF        %trim(%subst(vindta:598:4)) = 'ZESP'
047000100713     C                   EVAL      VABTSP='E'
047100100713     C                   ENDIF
047200100713     C                   IF        %trim(%subst(vindta:235:1)) = 'F'
047300100713     C                   EVAL      VABCBO='1'
047400100713     C                   ENDIF
047500100713     C                   IF        %trim(%subst(vindta:235:1)) = 'A'
047600100713     C                   EVAL      VABCBO='2'
047700100713     C                   ENDIF
047800100713     C                   IF        %trim(%subst(vindta:225:10)) = 'TASSATIVA'
047900100713     C                   EVAL      VABTCR=*blanks
048000100713     C                   ENDIF
048100100713     C                   IF        %trim(%subst(vindta:225:10)) = 'ENTRO IL'
048200100713     C                   EVAL      VABTCR='P'
048300100713     C                   ENDIF
048400100713     C                   IF        %trim(%subst(vindta:225:10)) = 'DOPO IL'
048500100713     C                   EVAL      VABTCR='D'
048600100713     C                   ENDIF
048700100713     C                   EVAL      wNOTE=%trim(%subst(vindta:247:75))+
048800100713     C                                   %trim(%subst(vindta:322:75))+
048900100713     C                                   %trim(%subst(vindta:397:75))+
049000100713     C                                   %trim(%subst(vindta:472:75))
049100100713     C                   EVAL      VABNOT=%subst(wNOTE:01:35)
049200100713     C                   EVAL      VABNT2=%subst(wNOTE:36:35)
049300100713     C*
049400100713     C* Considerazioni sulla ragione sociale del destinatario da indicare
049500100713     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
049600100713     C     '@':'A'       XLATE     VABRSD        VABRSD
049700100713     C* ==
049800100713     C*
049900100713     C* Reperimento campi NUMERICI
050000100713     C* NSP
050100100715     C                   EVAL      PiStr=%trim(%subst(vindta:38:10))
050200100713     C                   EXSR      CHKNUM
050300100713     C                   IF        PiInt=*on
050400100713     C                   Z-ADD     PiVal         VABNSP
050500100713     C                   ELSE
050600100713     C                   SETON                                        31
050700100713     C                   Z-ADD     *zeros        VABNSP
050800100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050900100713     C                             + ' ' + 'VABNSP'
051000100713     C                   ENDIF
051100100713     C* RMN
051200100713     C***                EVAL      PiStr=%trim(%subst(vindta:38:10))
051300100713     C                   EVAL      PiStr=%trim(%subst(vindta:20:10))
051400100713     C                   EXSR      CHKNUM
051500100713     C                   IF        PiInt=*on
051600100713     C                   Z-ADD     PiVal         VABRMN
051700100713     C                   ELSE
051800100713     C                   SETON                                        31
051900100713     C                   Z-ADD     1             VABRMN
052000100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052100100713     C                             + ' ' + 'VABRMN'
052200100713     C                   ENDIF
052300100713     C* CAD
052400100713     C                   EVAL      PiStr=%trim(%subst(vindta:189:10))
052500100713     C                   EXSR      CHKNUM
052600100713     C                   IF        PiInt=*on
052700100713     C                   Z-ADD     PiVal         Num5_0
052800100713     C                   MOVEL(p)  Num5_0        VABCAD
052900100713     C                   ELSE
053000100713     C                   SETON                                        32
053100100713     C                   EVAL      VABCAD = *zeros
053200100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053300100713     C                             + ' ' + 'VABCAD'
053400100713     C                   ENDIF
053500100713     C* Reperisco la provincia dal CAP e dalla località
053600100713     C                   IF        VABCAD <> *blanks AND
053700100713     C                             VABPRD  = *blanks
053800100713     C                   CLEAR                   TISI95DS
053900100713     C                   EVAL      I95TCN = '3'
054000100713     C                   Z-ADD     datcor        I95DAT
054100100713     C                   EVAL      I95CAP = VABCAD
054200100713     C                   EVAL      I95LOC = VABLOD
054300100713     C                   EVAL      I95NAR = VABNZD
054400100713     C                   CALL      'TISI95R'
054500100713     C                   PARM                    TISI95DS
054600100713     C                   EVAL      VABPRD = O95PRV
054700100713     C                   ENDIF
054800100713     C* DCR
054900100714     C                   IF        %subst(vindta:217:8) <> *blanks
055000100713     C                   EVAL      PiStr=%trim(%subst(vindta:217:8))
055100100713     C                   EXSR      CHKNUM
055200100713     C                   IF        PiInt=*on
055300100713     C                   Z-ADD     PiVal         VABDCR
055400100713     C                   ELSE
055500100713     C                   SETON                                        32
055600100713     C                   Z-ADD     *zeros        VABDCR
055700100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055800100713     C                             + ' ' + 'VABDCR'
055900100713     C                   ENDIF
056000100714     C                   ENDIF
056100100713     C* NCL
056200100713     C                   Z-ADD     *zeros        wNCL_01           5 0
056300100713     C                   Z-ADD     *zeros        wNCL_02           5 0
056400100713     C                   Z-ADD     *zeros        wNCL_03           5 0
056500100713     C                   Z-ADD     *zeros        wNCL_04           5 0
056600100713     C*
056700100713     C                   EVAL      PiStr=%trim(%subst(vindta:547:6))
056800100713     C                   EXSR      CHKNUM
056900100713     C                   IF        PiInt=*on
057000100713     C                   Z-ADD     PiVal         wNCL_01
057100100713     C                   ELSE
057200100713     C                   SETON                                        32
057300100713     C                   Z-ADD     *zeros        wNCL_01
057400100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
057500100713     C                             + ' ' + 'VABNCL'
057600100713     C                   ENDIF
057700100713     C*
057800100713     C                   EVAL      PiStr=%trim(%subst(vindta:553:6))
057900100713     C                   EXSR      CHKNUM
058000100713     C                   IF        PiInt=*on
058100100713     C                   Z-ADD     PiVal         wNCL_02
058200100713     C                   ELSE
058300100713     C                   SETON                                        32
058400100713     C                   Z-ADD     *zeros        wNCL_02
058500100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
058600100713     C                             + ' ' + 'VABNCL'
058700100713     C                   ENDIF
058800100713     C*
058900100713     C                   EVAL      PiStr=%trim(%subst(vindta:559:6))
059000100713     C                   EXSR      CHKNUM
059100100713     C                   IF        PiInt=*on
059200100713     C                   Z-ADD     PiVal         wNCL_03
059300100713     C                   ELSE
059400100713     C                   SETON                                        32
059500100713     C                   Z-ADD     *zeros        wNCL_03
059600100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
059700100713     C                             + ' ' + 'VABNCL'
059800100713     C                   ENDIF
059900100713     C*
060000100713     C                   EVAL      PiStr=%trim(%subst(vindta:565:6))
060100100713     C                   EXSR      CHKNUM
060200100713     C                   IF        PiInt=*on
060300100713     C                   Z-ADD     PiVal         wNCL_04
060400100713     C                   ELSE
060500100713     C                   SETON                                        32
060600100713     C                   Z-ADD     *zeros        wNCL_04
060700100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
060800100713     C                             + ' ' + 'VABNCL'
060900100713     C                   ENDIF
061000100713     C*
061100100713     C                   EVAL      VABNCL = VABNCL +
061200100713     C                                      wNCL_01+wNCL_02+wNCL_03+wNCL_04
061300100713     C* PKB
061400100713     C                   EVAL      PiStr=%trim(%subst(vindta:52:7))
061500100713     C                   EXSR      CHKNUM
061600100713     C                   IF        PiNum=*on
061700100713     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
061800100713     C                   EVAL      VABPKB = VABPKB + PiVal
061900100713     C                   ELSE
062000100713     C                   SETON                                        32
062100100713     C                   Z-ADD     *zeros        VABPKB
062200100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
062300100713     C                             + ' ' + 'VABPKB'
062400100713     C                   ENDIF
062500100713     C* CAS
062600100713     C                   IF        %trim(%subst(vindta:236:11))<>*all'0' AND
062700100713     C                             %trim(%subst(vindta:236:11))<>*blanks
062800100713     C                   EVAL      FlgCAS = '1'
062900100713     C                   EVAL      VABVCA='EUR'
063000100713     C                   EVAL      PiStr=%trim(%subst(vindta:236:11))
063100100713     C                   EXSR      CHKNUM
063200100713     C                   IF        PiNum=*on
063300100713     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
063400100713     C                   EVAL      VABCAS = VABCAS + PiVal
063500100713     C                   ELSE
063600100713     C                   SETON                                        32
063700100713     C                   Z-ADD     *zeros        VABCAS
063800100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
063900100713     C                             + ' ' + 'VABCAS'
064000100713     C                   ENDIF
064100100713     C                   ENDIF
064200070730     C*
064300070730     C* Considerazioni finali su CBO/CAS
064400100629     C                   IF        FlgCAS = '1'
064500100629     C                   IF        VABCBO = '1'
064600100629     C                   EVAL      VABCBO = '4'
064700100629     C                   ENDIF
064800100629     C                   IF        VABCBO = '2'
064900100629     C                   EVAL      VABCBO = '6'
065000100629     C                   ENDIF
065100100629     C                   ENDIF
065200010202     C*
065300000801     C* Ebbene...
065400000801     C                   ADD       1             §CTRMO
065500070530     C                   IF        *in31 <> *zeros OR
065600070530     C                             *in32 <> *zeros
065700000801     C                   ADD       1             §CTRNO
065800000801     C                   EVAL      recko = vindta
065900130114     C***                EXCEPT    rigadett
066000020725     C                   EVAL      x_vinflg = '2'
066100000801     C                   ELSE
066200010201     C                   ADD       1             §CTROKVB
066300000801     C                   ENDIF
066400000801     C*
066500000801     C                   ENDSR
066600100127     C*----------------------------------------------------*
066700100713     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "A"
066800100127     C*----------------------------------------------------*
066900100127     C     WRIVAT_A      BEGSR
067000100127     C*
067100100713     C* Valorizzo l buffer di scrittura del EDIVAT
067200100127     C* - TIPO RECORD "A"
067300100412     C***                eval      VATTRC = 'A'
067400100412     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
067500100412     C***                exsr      wrivat
067600100127     C*
067700100127     C                   ENDSR
067800010201     C*----------------------------------------------------*
067900100713     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "B"
068000010201     C*----------------------------------------------------*
068100071003     C     WRIVAT_B      BEGSR
068200010201     C*
068300100713     C* Valorizzo l buffer di scrittura del EDIVAT
068400070928     C* - TIPO RECORD "B"
068500100412     C***                eval      VATTRC = 'B'
068600100412     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
068700100412     C***                exsr      wrivat
068800010201     C*
068900010201     C                   ENDSR
069000071003     C*----------------------------------------------------*
069100100713     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
069200071003     C*----------------------------------------------------*
069300071003     C     WRIVAT_E      BEGSR
069400071003     C*
069500100713     C* Valorizzo l buffer di scrittura del EDIVAT
069600071003     C* - TIPO RECORD "E"
069700100629     C***                eval      VATTRC = 'E'
069800100629     C***                eval      VATNOT = %trim(%subst(vindta:14:6))
069900100629     C***                exsr      wrivat
070000071003     C*
070100071003     C                   ENDSR
070200100127     C*----------------------------------------------------*
070300100713     C*  SCARICO BUFFER EDIVAT
070400100127     C*----------------------------------------------------*
070500100127     C     WRIVAT        BEGSR
070600100127     C*
070700100127     C                   if        vatNOT <> *blanks
070800100127     C                   ADD       1             §CTROKVT
070900100713     C                   write     EDIVAT00
071000100127     C                   endif
071100100127     C*
071200100127     C                   ENDSR
071300020904
071400020904
071500010202     C*----------------------------------------------------*
071600100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
071700010202     C*----------------------------------------------------*
071800100525     C     PREVASX       BEGSR
071900100525     C*
072000100713     C* Compongo il nome del membro da dare al EDIVATWR
072100010202     C                   eval      parmbr = vlrhdl
072200010202     C                   movel     'M'           parmbr
072300070530     C                   eval      parccm = %subst(vlrKSC:2:7)
072400010202     C                   eval      paropz = '1'
072500100525     C*
072600010202     C* Effettuo la chiamata al CLLE preposto
072700100714     C  N51              call(e)   'TITVEVTC'
072800010202     C                   parm                    parccm
072900010202     C                   parm                    parmbr
073000010202     C                   parm                    paropz
073100100714     C   51              call(e)   'TITVEVBC'
073200100525     C                   parm                    parccm
073300100525     C                   parm                    parmbr
073400100525     C                   parm                    paropz
073500100525     C*
073600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
073700010202     C                   if        %error
073800010202     C                   movel     '1'           chkcall
073900010202     C                   else
074000010202     C                   movel     '0'           chkcall
074100010202     C                   endif
074200010202     C*
074300010202     C                   ENDSR
074400000801     C*----------------------------------------------------*
074500000801     C*  CONTROLLO NUMERICITA' CAMPI
074600000801     C*----------------------------------------------------*
074700000801     C     CHKNUM        BEGSR
074800081113     C*
074900081113     C                   IF        PiDecChr = *blanks
075000100503     C                   EVAL      PiDecChr = '.'
075100081113     C                   ENDIF
075200091223     C*
075300081113     C                   callp(e)  UBISNUM_Check(PiStr
075400081113     C                                          :PiDecChr
075500081113     C                                          :PiVal
075600081113     C                                          :PiNum
075700081113     C                                          :PiInt)
075800081113     C*
075900000801     C                   IF        %error
076000000801     C                   EVAL      PiInt=*off
076100000801     C                   ENDIF
076200000801     C*
076300000801     C                   ENDSR
076400000801     C***
076500011113
076600011113
076700000801
076800000801
076900990920      /TITLE Invio dei dati al punto operativo.
077000010202     C     invio         BEGSR
077100990920     C*
077200100713     C* 1° invio EDIVAT
077300010201     C                   reset                   dscmz
077400010201     C                   move      vlrpoi        cmzdst
077500100713     C                   eval      cmzfld = 'EDIVATWR'
077600010201     C                   eval      cmzmbd = vlrhdl
077700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
077800021009     C***                if        prmfir = *blanks
077900100713     C                   eval      cmzfla = 'EDIVAT0F'
078000100713     C                   eval      cmzmba = 'EDIVAT0F'
078100021009     C***                else
078200021009     C***                eval      cmzfla = prmfir
078300021009     C***                eval      cmzmba = prmfir
078400021009     C***                endif
078500010201     C                   eval      cmznrr = *zeros
078600020305     C                   move      §ctrokvt      cmznrr
078700021018     C                   eval      cmzlba = vlrfl1
078800010201     C                   call(e)   'TIS711C'
078900010201     C                   parm                    dscmz
079000010201     C                   parm      *blanks       esito
079100010205     C                   if        %error
079200010205     C                             or cmzerr = '1'
079300010205     C                             or esito  = '1'
079400010205     C                   eval      wrkesito = '3'
079500010205     C                   else
079600010201     C*
079700100713     C* 2° invio EDIVAB
079800100525     C                   if        *in51 = *off
079900010201     C                   reset                   dscmz
080000010201     C                   move      vlrpoi        cmzdst
080100010201     C                   eval      cmzfld = vlrfou
080200010201     C                   eval      cmzmbd = vlrhdl
080300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
080400021009     C***                if        prmfir = *blanks
080500100713     C                   eval      cmzfla = 'EDIVAB0F'
080600100713     C                   eval      cmzmba = 'EDIVAB0F'
080700021009     C***                else
080800021009     C***                eval      cmzfla = prmfir
080900021009     C***                eval      cmzmba = prmfir
081000021009     C***                endif
081100010201     C                   eval      cmznrr = *zeros
081200010201     C                   move      §ctrokvb      cmznrr
081300021018     C                   eval      cmzlba = vlrfl1
081400010201     C                   call(e)   'TIS711C'
081500010201     C                   parm                    dscmz
081600010201     C                   parm      *blanks       esito
081700010201     C                   if        %error
081800010201     C                             or cmzerr = '1'
081900010201     C                             or esito  = '1'
082000010201     C                   eval      wrkesito = '3'
082100010201     C                   endif
082200100525     C                   endif
082300010205     C                   endif
082400990920     C*
082500000613     C                   ENDSR
082600000613     C***
082700070411
082800070411     C     *pssr         BEGSR
082900070411     C*
083000070411     C                   if        %open(tivin00r)
083100070411     C                   close     tivin00r
083200070411     C                   endif
083300100713     C                   if        %open(edivabwr)
083400100713     C                   close     edivabwr
083500070411     C                   endif
083600100713     C                   if        %open(edivatwr)
083700100713     C                   close     edivatwr
083800070411     C                   endif
083900070411     C*
084000070411     C* Effettuo la chiamata al CLLE preposto
084100100714     C  N51              call(e)   'TITVEVTC'
084200100525     C                   parm                    parccm
084300100525     C                   parm                    parmbr
084400100525     C                   parm      '2'           paropz
084500100714     C   51              call(e)   'TITVEVBC'
084600100525     C                   parm                    parccm
084700100525     C                   parm                    parmbr
084800100525     C                   parm      '2'           paropz
084900070411     C*
085000070411     C                   eval      wrkesito = '2'
085100070411     C*
085200070411     C                   seton                                        LR
085300070411     C*
085400070411     C                   ENDSR     '*CANCL'
085500070411     C***
085600070411
085700990910
085800000613     C     *inzsr        BEGSR
085900990910     C*
086000990910     C     *entry        plist
086100990920     C                   parm                    tivlrds
086200990921     C                   parm      wrkesito      esito
086300000724     C                   parm                    prmlit
086400000710     C                   parm                    prmfir
086500100713     C*
086600100713     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
086700100713     C                   move(p)   vlrMSG        wNomeFile
086800000613     C*
086900000830     C* CALCOLA LA DATA CORRENTE
087000100713     C                   time                    wn14             14 0
087100100713     C                   movel     wn14          oracor            6 0          *ORA
087200091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
087300091223     C                   eval      datcor = %dec(%date() : *ISO)
087400000830     C*
087500000613     C                   ENDSR
087600000613     C***
087700990908
087800100629     Otitv1y4p  E            riepilogo         2
087900990915     O                                              'Upload via Internet'
088000990915     O                                           +1 'Traduzione TIVIN00R -'
088100100713     O                                           +1 'EDIVABWR/EDIVATWR'
088200010201     O                                           +1 'Report testate bolle'
088300990915     O          E            riepilogo   2
088400990915     O                       wrkdata
088500990915     O                       wrkora              +1
088600990915     O                       procname            +1
088700990915     O          E            riepilogo   2
088800990915     O                                              'Cliente..................:'
088900990915     O                       VABCCM        z     +1
089000990915     O          E            riepilogo   2
089100990920     O                                              'Riferimento Strategi.....:'
089200990920     O                       vlrhdl              +1
089300990915     O          E            riepilogo   2
089400990915     O                                              'Giusti...................:'
089500010201     O                       §CTROKVB      2   +  1
089600100525     O                       §CTROKVT      2   +  1
089700990915     O          E            riepilogo   2
089800990915     O                                              'Sbagliati e corretti.....:'
089900971022     O                       §CTRMO        2   +  1
090000990915     O          E            riepilogo   2
090100990915     O                                              'Sbagliati e scartati.....:'
090200971022     O                       §CTRNO        2   +  1
090300000613
090400100629     Otitv1y4ps E            testdett          2
090500000613     O                                              'Upload via Internet'
090600000613     O                                           +1 'Traduzione TIVIN00R -'
090700100713     O                                           +1 'EDIVABWR/EDIVATWR'
090800010201     O                                           +1 'Report testate bolle'
090900000616     O          E            testdett    3
091000000613     O                                           +2 'N° rec'
091100000613     O                                           +3 'Anteprima contenuto'
091200000616     O          E            rigadett    2
091300000613     O                       rrnum               +2
091400000621     O                       recko               +3
091500000616     O          E            findett     2
091600000613     O                       wrkdata
091700000613     O                       wrkora              +1
091800000613     O                       procname            +1
091900000616     O          E            findett     2
092000000613     O                                              'Cliente..................:'
092100000613     O                       VABCCM        z     +1
092200000616     O          E            findett     2
092300000613     O                                              'Riferimento Strategi.....:'
092400000613     O                       vlrhdl              +1
092500000616     O          E            findett     2
092600000613     O                                              'Giusti...................:'
092700010201     O                       §CTROKVB      2   +  1
092800100525     O                       §CTROKVT      2   +  1
092900000616     O          E            findett     2
093000000613     O                                              'Sbagliati e corretti.....:'
093100000613     O                       §CTRMO        2   +  1
093200000616     O          E            findett     2
093300000613     O                                              'Sbagliati e scartati.....:'
093400000613     O                       §CTRNO        2   +  1
093500000512** CMD - COMANDI CL
093600100629OVRPRTF FILE(TITV1Y4P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
093700100629OVRPRTF FILE(TITV1Y4PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
093800100629DLTOVR FILE(TITV1Y4P TITV1Y4PS) LVL(*)
093900000512
094000000512
