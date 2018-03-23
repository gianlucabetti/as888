000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800021017     Ftitv26p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100021017     Ftitv26ps  O    f  198        PRINTER usropn
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
002600990910     D esito           s              1
002700000724     D prmlit          s             10
002800000710     D prmfir          s             10
002900990921     D wrkesito        s                   like(esito)
003000990915     D wrkdata         s               d
003100990915     D wrkora          s               t
003200000613     D rrnum           s              6  0 INZ(*zeros)
003300000621     D recko           s            150    INZ(*blanks)
003400011113     D depcmd          s            150    INZ(*blanks)
003500020305     D depspe          s             14    INZ(*blanks)
003600010213     D depdat          s              8  0 INZ(*zeros)
003700020305     D curspe          s             14    INZ(*blanks)
003800010202     D parccm          s              8    INZ(*blanks)
003900010202     D parmbr          s             10    INZ(*blanks)
004000010202     D paropz          s              1    INZ(*blanks)
004100010202     D chkcall         s              1    INZ(*blanks)
004200010213
004300000801     D  Num5_0         s              5  0
004400000830
004500000830     D*------------------
004600000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004700000830     D*------------------
004800000830     D WLBDA8          DS                  INZ
004900000830     D  G08DAT                 1      8  0
005000000830     D  G08INV                 9     16  0
005100000830     D  G08ERR                17     17
005200000830     D  G08TGI                18     22  0
005300000830     D*
005400990908
005500010201
005600010201
005700990915     C                   time                    wrkdata
005800990915     C                   time                    wrkora
005900000913     C                   reset                   rrnum
006000990921     C                   reset                   esito
006100990921     C                   reset                   wrkesito
006200000724     C*
006300000724     C* SE OCCORRE SPEDIRE IN FILIALE
006400021017     C                   if        vlrpoi <> *zeros
006500000724     C*
006600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006800021017     C     vlrpoi        chain     azorg01l
006900000724     C                   if        %found
007000000616     C                   movel(p)  CMD(1)        depcmd
007100020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007200000616     C*
007300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007400011113     C                   Z-ADD     150           LENGH            15 5
007500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007600000616     C                   PARM                    depcmd
007700000616     C                   PARM                    LENGH
007800000724     C*
007900000724     C                   endif
008000000724     C                   endif
008100000616     C*
008200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008300000616     C                   movel(p)  CMD(2)        depcmd
008400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008500000616     C*
008600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008700011113     C                   Z-ADD     150           LENGH            15 5
008800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008900000616     C                   PARM                    depcmd
009000000616     C                   PARM                    LENGH
009100000616     C*
009200021017     C                   if        not %open(titv26ps)
009300021017     C                   open      titv26ps
009400000616     C                   except    testdett
009500000613     C                   endif
009600000613     C*
009700021017     C                   EXSR      RWMEDIOL                                     LETT/SCR. VAB
009800000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009900000613     C*
010000010202     C* Effettuo la chiamata al CLLE preposto
010100050128     C                   call(e)   'TITVVTC'
010200010202     C                   parm                    parccm
010300010202     C                   parm                    parmbr
010400010202     C                   parm      '2'           paropz
010500010202     C*
010600021017     C                   if        %open(titv26ps)
010700000616     C                   except    findett
010800021017     C                   close     titv26ps
010900000613     C                   endif
011000000616     C*
011100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011200011113     C                   Z-ADD     150           LENGH            15 5
011300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011400000616     C                   PARM                    CMD(3)
011500000616     C                   PARM                    LENGH
011600000616     C*
011700000801     C
011800010201     C                   seton                                        LR
011900000613
012000000613
012100000613     C*--------------------------------------------------------
012200000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012300000613     C*--------------------------------------------------------
012400000621     C     STPR          BEGSR
012500000613     C*
012600021017     C                   if        not %open(titv26p)
012700021017     C                   open      titv26p
012800990915     C                   endif
012900990915     C*
013000990915     C                   except    riepilogo
013100990915     C*
013200021017     C                   if        %open(titv26p)
013300021017     C                   close     titv26p
013400990914     C                   endif
013500990910     C*
013600000613     C                   ENDSR
013700000613     C***
013800990908
013900000801
014000910830     C*--------------------------------------------------------
014100021017     C* RWMEDIOL LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
014200910830     C*--------------------------------------------------------
014300021017     C     RWMEDIOL      BEGSR
014400990910     C*
014500990914     C                   if        not %open(tivin00r)
014600990908     C                   open      tivin00r
014700990914     C                   endif
014800021017     C                   if        not %open(fivabwwr)
014900021017     C                   open      fivabwwr
015000990914     C                   endif
015100021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015200020305     C                   exsr      prevat
015300010201     C*
015400010202     C                   if        chkcall = '0'
015500010202     C*
015600021017     C                   if        not %open(fivatwwr)
015700021017     C                   open      fivatwwr
015800010201     C                   endif
015900990910     C*
016000010201     C                   clear                   §CTROKVB          5 0
016100020305     C                   clear                   §CTROKVT          5 0
016200000801     C                   clear                   §CTRMO            5 0
016300000801     C                   clear                   §CTRNO            5 0
016400990910     C*
016500921023     C                   DO        *HIVAL
016600990913     C*
016700990915     C                   READ      tivin00r                               70
016800000905     C                   if        vindta > *blanks
016900000613     C                   add       1             rrnum
017000000801     C*
017100000801     C                   if        *in70 = *off
017200000801     C                             and
017300000801     C                             (vinflg = *blanks
017400000801     C                              or vinflg = '0'
017500000801     C                              or vinflg = '2')
017600000801     C*
017700000801     C                   clear                   vinmsg
017800010305     C*
017900010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
018000021017     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
018100020305     C                   MOVEL(p)  PiStr         curspe
018200010305     C*
018300010305     C                   if        depspe = *blanks                             => 1° giro
018400010305     C                   eval      depspe = curspe                              => memorizz. spediz
018500020305     C                   exsr      impvab
018600020305     C                   exsr      wrivat                                       => carico VAT
018700010305     C                   else
018800010305     C                   if        curspe <> depspe                             => rottura di spediz
018900010305     C                   eval      depspe = curspe                              => memorizz. spediz
019000020305     C                   exsr      wrivab
019100020305     C                   exsr      impvab
019200020305     C                   exsr      wrivat                                       => carico VAT
019300020305     C                   else                                                   => x stessa spediz
019400020305     C                   exsr      impvab
019500020305     C                   exsr      wrivat                                       => carico VAT
019600010305     C                   endif
019700010305     C                   endif
019800010305     C                   endif
019900000905     C*
020000000905     C                   else
020100000905     C                   eval      vinflg = '1'
020200000905     C                   endif
020300000905     C*
020400000905     C  N70              update    tivin000
020500000905     C*
020600991022     C  N70              ENDdo
020700010202     C*
020800020305     C* Scarico i VAB rimasti "in sospeso"
020900020305     C                   exsr      wrivab
021000010202     C*
021100010202     C                   endif
021200990910
021300990910     C* Se non ci sono record con errori ...
021400000710     C                   if        §ctrno = 0
021500990910     C* ... restituisco esito OK.
021600990921     C                   eval      wrkesito = '0'
021700990910     C                   else
021800010201     C                   if        §ctrokvb > 0
021900990921     C                   eval      wrkesito = '1'
022000000710     C                   else
022100000710     C                   eval      wrkesito = '2'
022200990910     C                   endif
022300000710     C                   endif
022400990910     C*
022500990914     C                   if        %open(tivin00r)
022600990908     C                   close     tivin00r
022700990914     C                   endif
022800021017     C                   if        %open(fivabwwr)
022900021017     C                   close     fivabwwr
023000990914     C                   endif
023100021017     C                   if        %open(fivatwwr)
023200021017     C                   close     fivatwwr
023300010201     C                   endif
023400021017     C*
023500021017     C                   if        vlrpoi <> 999
023600021017     C                   eval      vlrpoi = vlrpoi
023700021017     C                   endif
023800990910     C*
023900010201     C                   if        §ctrokvb > 0
024000021017     C                             and vlrpoi > *zeros
024100010202     C                   exsr      invio
024200990920     C                   endif
024300990920     C*
024400910830     C                   ENDSR
024500000613     C***
024600010305
024700010305     C*----------------------------------------------------*
024800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
024900010305     C*----------------------------------------------------*
025000020305     C     WRIVAB        BEGSR
025100010305     C*
025200021017     C                   write     fivab000                                     => scarico il VAB
025300010305     C*
025400010305     C                   ENDSR
025500990920
025600000801     C*----------------------------------------------------*
025700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
025800000801     C*----------------------------------------------------*
025900010201     C     INZVAR        BEGSR
026000000801     C*
026100010201     C                   Z-ADD     *zeros        Num5_0
026200000801     C*
026300000801     C                   ENDSR
026400000801     C*----------------------------------------------------*
026500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
026600000801     C*----------------------------------------------------*
026700000801     C     DEFCAM        BEGSR
026800000801     C*
026900021017     C                   CLEAR                   FIVAB000
027000020619     C* Imposto i valori di default...
027100021017     C                   Z-ADD     0020950       VABCCM
027200021017     C                   Z-ADD     0020950       VATCCM
027300021017     C                   Z-ADD     vlrpoi        VABFGS
027400021017     C                   Z-ADD     vlrpoi        VATFGS
027500021017     C                   Z-ADD     002           VABLNP
027600021017     C                   Z-ADD     002           VATLNP
027700021017     C                   Z-ADD     300           VABCTR
027800021017     C                   Z-ADD     1             VABNCL
027900021017     C                   Z-ADD     1             VABPKB
028000021017     C                   MOVEL     '7Q'          VABCTM
028100021120     C***                MOVEL     'A'           VABTC1
028200020305     C                   MOVEL     'E'           VATTRC
028300020619     C* ... e poi verifico se sono stati passati come parametri
028400020619     C                   IF        vlrppt > *blanks
028500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028600020619     C                   EXSR      CHKNUM
028700020619     C                   IF        PiInt=*on
028800020619     C                   Z-ADD     PiVal         VABCCM
028900020619     C                   Z-ADD     PiVal         VATCCM
029000020619     C                   ENDIF
029100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029200020619     C                   EXSR      CHKNUM
029300020619     C                   IF        PiInt=*on
029400020619     C                   Z-ADD     PiVal         VABLNP
029500020619     C                   Z-ADD     PiVal         VATLNP
029600020619     C                   ENDIF
029700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
029800020619     C                   EXSR      CHKNUM
029900020619     C                   IF        PiInt=*on
030000020619     C                   Z-ADD     PiVal         VABCTR
030100020619     C                   ENDIF
030200020619     C                   ENDIF
030300000801     C*
030400000801     C                   ENDSR
030500000801     C*----------------------------------------------------*
030600021017     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
030700000801     C*----------------------------------------------------*
030800010201     C     IMPVAB        BEGSR
030900000801     C*
031000020305     C                   EXSR      INZVAR
031100020305     C                   EXSR      DEFCAM
031200010305     C*
031300000801     C                   Z-ADD     *zeros        errore            1 0
031400000830     C                   MOVEL     datcor        VABAAS
031500020305     C                   MOVEL     datcor        VATAAS
031600000830     C                   MOVE      datcor        VABMGS
031700021017     C                   EVAL      VABRSD=%trim(%subst(vindta:43:35))
031800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
031900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032000020117     C* ==
032100050525     C                   EVAL      VABRD2=%trim(%subst(vindta:78:35))
032200050525     C                   EVAL      VABIND=%trim(%subst(vindta:113:35))
032300050525     C                   EVAL      VABLOD=%trim(%subst(vindta:148:35))
032400050525     C                   EVAL      VABPRD=%trim(%subst(vindta:193:2))
032500050915     C                   EVALR     VABRMA=%trim(%subst(vindta:8:35))
032600050525     C                   EVAL      VABNOT=%trim(%subst(vindta:205:35))
032700050525     C                   EVAL      VABNT2=%trim(%subst(vindta:240:35))
032800020305     C*
032900050525     C                   EVAL      PiStr=%trim(%subst(vindta:183:10))
033000010201     C                   EXSR      CHKNUM
033100010201     C                   IF        PiInt=*on
033200010201     C                   Z-ADD     PiVal         Num5_0
033300021017     C                   MOVEL     Num5_0        VABCAD
033400010201     C                   ELSE
033500010201     C                   ADD       1             errore
033600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
033700021017     C                             + ' ' + 'VABCAD'
033800010201     C                   ENDIF
033900020619     C*
034000021017     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
034100010201     C                   EXSR      CHKNUM
034200010201     C                   IF        PiInt=*on
034300011115     C                   Z-ADD     PiVal         VABNSP
034400020305     C                   Z-ADD     PiVal         VATNSP
034500021017     C                   Z-ADD     PiVal         VABRMN
034600010201     C                   ELSE
034700010201     C                   ADD       1             errore
034800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034900021017     C                             + ' ' + 'VABNSP VATNSP VABRMN'
035000010201     C                   ENDIF
035100010205     C*
035200010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
035300010205     C                   IF        VABCAS > *zeros
035400010205     C                   EVAL      VABCBO = '4'
035500010205     C                   ELSE
035600010205     C                   EVAL      VABCBO = '1'
035700010205     C                   ENDIF
035800020305     C*
035900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
036000011113     C                   EXSR      CHKIMPDIV
036100010202     C*
036200000801     C* Ebbene...
036300000801     C                   ADD       1             §CTRMO
036400010201     C                   IF        errore <> *zeros
036500000801     C                   ADD       1             §CTRNO
036600000801     C                   EVAL      recko = vindta
036700000801     C                   EXCEPT    rigadett
036800000801     C                   EVAL      vinflg = '2'
036900000801     C                   ELSE
037000010201     C                   ADD       1             §CTROKVB
037100000801     C                   ENDIF
037200000801     C*
037300000801     C                   ENDSR
037400010201     C*----------------------------------------------------*
037500021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
037600010201     C*----------------------------------------------------*
037700020305     C     WRIVAT        BEGSR
037800010201     C*
037900021017     C* Valorizzo l buffer di scrittura del FIVAT
038000021017     C                   EVAL      VATNOT = %subst(vindta:8:35)
038100021017     C                   write     FIVAT000
038200010201     C*
038300010201     C                   ENDSR
038400010202     C*----------------------------------------------------*
038500021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
038600010202     C*----------------------------------------------------*
038700020305     C     PREVAT        BEGSR
038800010202     C*
038900021017     C* Compongo il nome del membro da dare al FIVATWWR
039000010202     C                   eval      parmbr = vlrhdl
039100010202     C                   movel     'M'           parmbr
039200021017     C                   eval      parccm = '00020950'
039300010202     C                   eval      paropz = '1'
039400010202     C* Effettuo la chiamata al CLLE preposto
039500050128     C                   call(e)   'TITVVTC'
039600010202     C                   parm                    parccm
039700010202     C                   parm                    parmbr
039800010202     C                   parm                    paropz
039900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
040000010202     C                   if        %error
040100010202     C                   movel     '1'           chkcall
040200010202     C                   else
040300010202     C                   movel     '0'           chkcall
040400010202     C                   endif
040500010202     C*
040600010202     C                   ENDSR
040700000801     C*----------------------------------------------------*
040800000801     C*  CONTROLLO NUMERICITA' CAMPI
040900000801     C*----------------------------------------------------*
041000000801     C     CHKNUM        BEGSR
041100000801     C*
041200000801     C                   call(e)   'ISNUMERIC'
041300000801     C                   PARM                    PiStr            30
041400020619     C                   PARM      '.'           PiDecChr          1
041500000801     C                   PARM      *ZEROS        PiVal            30 9
041600000801     C                   PARM      '0'           PiInt             1
041700000801     C                   PARM      '0'           PiNum             1
041800000801     C                   IF        %error
041900000801     C                   EVAL      PiInt=*off
042000000801     C                   ENDIF
042100000801     C*
042200000801     C                   ENDSR
042300000801     C***
042400000801
042500011113
042600011113     C*----------------------------------------------------*
042700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
042800011113     C*----------------------------------------------------*
042900011113     C     CHKIMPDIV     BEGSR
043000011113     C*
043100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
043200011113     C                   Z-ADD     *zeros        wrkDec            9 9
043300011113     C*
043400011113     C* Come prima cosa effettuo considerazioni sulla divisa
043500011113     C                   IF        vabIAS > *zeros
043600011113     C                   IF        vabVAS <> 'EUR'
043700011113     C                   EVAL      vabVAS =  'ITL'
043800011113     C                   ENDIF
043900011113     C                   ENDIF
044000011113     C*
044100011113     C                   IF        vabCAS > *zeros
044200011113     C                   IF        vabVCA <> 'EUR'
044300011113     C                   EVAL      vabVCA =  'ITL'
044400011113     C                   ENDIF
044500011113     C                   ENDIF
044600011113     C*
044700011113     C                   IF        vabVMD > *zeros
044800020305     C                   IF        vabVAD <> 'EUR'
044900011113     C                   EVAL      vabVAD =  'ITL'
045000011113     C                   ENDIF
045100011113     C                   ENDIF
045200011113     C*
045300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
045400011113     C                   Z-ADD     vabIAS        wrkDec
045500011113     C                   IF        wrkDec > *zeros
045600011113     C                   IF        vabVAS = 'ITL'
045700011113     C                   EVAL      vabIAS = *zeros
045800011113     C                   ENDIF
045900011113     C                   ENDIF
046000011113     C*
046100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
046200011113     C                   Z-ADD     vabCAS        wrkDec
046300011113     C                   IF        wrkDec > *zeros
046400011113     C                   IF        vabVCA = 'ITL'
046500011113     C                   EVAL      vabCAS = *zeros
046600011113     C                   ENDIF
046700011113     C                   ENDIF
046800011113     C*
046900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
047000011113     C                   Z-ADD     vabVMD        wrkDec
047100011113     C                   IF        wrkDec > *zeros
047200011113     C                   IF        vabVAD = 'ITL'
047300011113     C                   EVAL      vabVMD = *zeros
047400011113     C                   ENDIF
047500011113     C                   ENDIF
047600011113     C*
047700011113     C                   ENDSR
047800011113     C***
047900021017
048000000801
048100000801
048200990920      /TITLE Invio dei dati al punto operativo.
048300010202     C     invio         BEGSR
048400990920     C*
048500021017     C* 1° invio FIVAT
048600010201     C                   reset                   dscmz
048700021017     C                   move      vlrpoi        cmzdst
048800021017     C                   eval      cmzfld = 'FIVATWWR'
048900010201     C                   eval      cmzmbd = vlrhdl
049000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
049100021009     C***                if        prmfir = *blanks
049200021017     C                   eval      cmzfla = 'FIVAT00F'
049300021017     C                   eval      cmzmba = 'FIVAT00F'
049400021009     C***                else
049500021009     C***                eval      cmzfla = prmfir
049600021009     C***                eval      cmzmba = prmfir
049700021009     C***                endif
049800010201     C                   eval      cmznrr = *zeros
049900020305     C                   move      §ctrokvt      cmznrr
050000021018     C                   eval      cmzlba = vlrfl1
050100010201     C                   call(e)   'TIS711C'
050200010201     C                   parm                    dscmz
050300010201     C                   parm      *blanks       esito
050400010205     C                   if        %error
050500010205     C                             or cmzerr = '1'
050600010205     C                             or esito  = '1'
050700010205     C                   eval      wrkesito = '3'
050800010205     C                   else
050900010201     C*
051000021017     C* 2° invio FIVAB
051100010201     C                   reset                   dscmz
051200021017     C                   move      vlrpoi        cmzdst
051300010201     C                   eval      cmzfld = vlrfou
051400010201     C                   eval      cmzmbd = vlrhdl
051500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
051600021009     C***                if        prmfir = *blanks
051700021017     C                   eval      cmzfla = 'FIVAB00F'
051800021017     C                   eval      cmzmba = 'FIVAB00F'
051900021009     C***                else
052000021009     C***                eval      cmzfla = prmfir
052100021009     C***                eval      cmzmba = prmfir
052200021009     C***                endif
052300010201     C                   eval      cmznrr = *zeros
052400010201     C                   move      §ctrokvb      cmznrr
052500021018     C                   eval      cmzlba = vlrfl1
052600010201     C                   call(e)   'TIS711C'
052700010201     C                   parm                    dscmz
052800010201     C                   parm      *blanks       esito
052900010201     C                   if        %error
053000010201     C                             or cmzerr = '1'
053100010201     C                             or esito  = '1'
053200010201     C                   eval      wrkesito = '3'
053300010201     C                   endif
053400010205     C                   endif
053500990920     C*
053600000613     C                   ENDSR
053700000613     C***
053800070411
053900070411     C     *pssr         BEGSR
054000070411     C*
054100070411     C                   if        %open(tivin00r)
054200070411     C                   close     tivin00r
054300070411     C                   endif
054400070411     C                   if        %open(fivabwwr)
054500070411     C                   close     fivabwwr
054600070411     C                   endif
054700070411     C                   if        %open(fivatwwr)
054800070411     C                   close     fivatwwr
054900070411     C                   endif
055000070411     C*
055100070411     C* Effettuo la chiamata al CLLE preposto
055200070411     C                   call(e)   'TITVVTC'
055300070411     C                   parm                    parccm
055400070411     C                   parm                    parmbr
055500070411     C                   parm      '2'           paropz
055600070411     C*
055700070411     C                   eval      wrkesito = '2'
055800070411     C*
055900070411     C                   seton                                        LR
056000070411     C*
056100070411     C                   ENDSR     '*CANCL'
056200070411     C***
056300070411
056400990910
056500000613     C     *inzsr        BEGSR
056600990910     C*
056700990910     C     *entry        plist
056800990920     C                   parm                    tivlrds
056900990921     C                   parm      wrkesito      esito
057000000724     C                   parm                    prmlit
057100000710     C                   parm                    prmfir
057200000613     C*
057300000830     C* CALCOLA LA DATA CORRENTE
057400000830     C                   time                    wn14             14 0
057500000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
057600000830     C                   z-add     wn8           g08dat
057700000830     C                   z-add     *zeros        g08inv
057800000830     C                   movel     '0'           g08err
057900000830     C                   call      'XSRDA8'
058000000830     C                   parm                    wlbda8
058100000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
058200000830     C*
058300000613     C                   ENDSR
058400000613     C***
058500990908
058600021017     Otitv26p   E            riepilogo         2
058700990915     O                                              'Upload via Internet'
058800990915     O                                           +1 'Traduzione TIVIN00R -'
058900021017     O                                           +1 'FIVABWWR/FIVATWWR'
059000010201     O                                           +1 'Report testate bolle'
059100990915     O          E            riepilogo   2
059200990915     O                       wrkdata
059300990915     O                       wrkora              +1
059400990915     O                       procname            +1
059500990915     O          E            riepilogo   2
059600990915     O                                              'Cliente..................:'
059700990915     O                       VABCCM        z     +1
059800990915     O          E            riepilogo   2
059900990920     O                                              'Riferimento Strategi.....:'
060000990920     O                       vlrhdl              +1
060100990915     O          E            riepilogo   2
060200990915     O                                              'Giusti...................:'
060300010201     O                       §CTROKVB      2   +  1
060400990915     O          E            riepilogo   2
060500990915     O                                              'Sbagliati e corretti.....:'
060600971022     O                       §CTRMO        2   +  1
060700990915     O          E            riepilogo   2
060800990915     O                                              'Sbagliati e scartati.....:'
060900971022     O                       §CTRNO        2   +  1
061000000613
061100021017     Otitv26ps  E            testdett          2
061200000613     O                                              'Upload via Internet'
061300000613     O                                           +1 'Traduzione TIVIN00R -'
061400021017     O                                           +1 'FIVABWWR/FIVATWWR'
061500010201     O                                           +1 'Report testate bolle'
061600000616     O          E            testdett    3
061700000613     O                                           +2 'N° rec'
061800000613     O                                           +3 'Anteprima contenuto'
061900000616     O          E            rigadett    2
062000000613     O                       rrnum               +2
062100000621     O                       recko               +3
062200000616     O          E            findett     2
062300000613     O                       wrkdata
062400000613     O                       wrkora              +1
062500000613     O                       procname            +1
062600000616     O          E            findett     2
062700000613     O                                              'Cliente..................:'
062800000613     O                       VABCCM        z     +1
062900000616     O          E            findett     2
063000000613     O                                              'Riferimento Strategi.....:'
063100000613     O                       vlrhdl              +1
063200000616     O          E            findett     2
063300000613     O                                              'Giusti...................:'
063400010201     O                       §CTROKVB      2   +  1
063500000616     O          E            findett     2
063600000613     O                                              'Sbagliati e corretti.....:'
063700000613     O                       §CTRMO        2   +  1
063800000616     O          E            findett     2
063900000613     O                                              'Sbagliati e scartati.....:'
064000000613     O                       §CTRNO        2   +  1
064100000512** CMD - COMANDI CL
064200021017OVRPRTF FILE(TITV26P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
064300021017OVRPRTF FILE(TITV26PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
064400021017DLTOVR FILE(TITV26P TITV26PS) LVL(*)
064500000512
064600000512
