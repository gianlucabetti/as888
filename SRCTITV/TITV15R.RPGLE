000100030506      /TITLE Upload via Internet: traduzione in FIVABWWR. (x NETSIEL - Cod. 0632629 (ex:0632157))
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500041216     Ftntbe01l  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700021025     FFIVABwwr  O    E             DISK    usropn
000800040302     FFIVATwwr  O    E             DISK    usropn
000900020604     Ftitv15p   O    f  132        PRINTER usropn
001000000621     F                                     oflind(*inoa)
001100020604     Ftitv15ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300000313     D*
001400000613     D*------------
001500000613     D* COMANDI
001600000613     D*------------
001700011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001800010330     D*----------------------------------------------------
001900010330     D* DICHIARAZIOINE VARIABILI DI WRK
002000010330     D*----------------------------------------------------
002100010330     D dscmz         e ds                  inz
002200010330     D psds           sds
002300010330     D  procname         *PROC
002400010330     D tivlrds       e ds                  extname(tivlr00f)
002500041216     D fivabds       e ds                  extname(fivab00f)
002600041216     D dosr          e ds                  inz
002700080502     d aD§OSRcl6                      7A   overlay(D§OSRcl6)
002800080502     d aD§OSRcl7                      7A   overlay(D§OSRcl7)
002900080502     d aD§OSRcl8                      7A   overlay(D§OSRcl8)
003000080502     d aD§OSRcl9                      7A   overlay(D§OSRcl9)
003100080502     d aD§OSRcl10                     7A   overlay(D§OSRcl10)
003200080502     d aD§OSRcl11                     7A   overlay(D§OSRcl11)
003300080502     d aD§OSRcl12                     7A   overlay(D§OSRcl12)
003400080502     d aD§OSRcl13                     7A   overlay(D§OSRcl13)
003500080502     d aD§OSRcl14                     7A   overlay(D§OSRcl14)
003600080502     d aD§OSRcl15                     7A   overlay(D§OSRcl15)
003700080502     d aD§OSRcl16                     7A   overlay(D§OSRcl16)
003800010330     D esito           s              1
003900010330     D prmlit          s             10
004000010330     D prmfir          s             10
004100010330     D wrkesito        s                   like(esito)
004200010330     D wrkdata         s               d
004300010330     D wrkora          s               t
004400010330     D rrnum           s              6  0 INZ(*zeros)
004500010330     D recko           s            150    INZ(*blanks)
004600011113     D depcmd          s            150    INZ(*blanks)
004700020604     D countrec        s              6  0 INZ(*zeros)
004800040302     D parccm          s              8    INZ(*blanks)
004900040302     D parmbr          s             10    INZ(*blanks)
005000040302     D paropz          s              1    INZ(*blanks)
005100040302     D chkcall         s              1    INZ(*blanks)
005200040302     D wNSPPRG1        s              3  0 INZ(*zeros)
005300040302     D wNSPPRG2        s              4  0 INZ(*zeros)
005400041216     D wNSR            s              2    INZ
005500041216     D wfivabds        s                   LIKE(fivabds)
005600010330     D*------------------
005700010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005800010330     D*------------------
005900010330     D WLBDA8          DS                  INZ
006000010330     D  G08DAT                 1      8  0
006100010330     D  G08INV                 9     16  0
006200010330     D  G08ERR                17     17
006300010330     D  G08TGI                18     22  0
006400080502     D*------------------
006500080502     D* SCHIERA X REPERIMENTO SERIE ORM DA TABELLA "OSR"
006600080502     D*------------------
006700080502     D SkCliOSR        s             10    DIM(900)
006800080502     D*
006900080502     D DsCliOSR        ds
007000080502     D  wLnpOSR                       3
007100080502     D  wCliOSR                       7
007200080502     D*
007300080502     D SkLnpNrsOSR     s              5    DIM(900)
007400010330
007500010330
007600010330
007700990915     C                   time                    wrkdata
007800990915     C                   time                    wrkora
007900000913     C                   reset                   rrnum
008000990921     C                   reset                   esito
008100990921     C                   reset                   wrkesito
008200010601     C*
008300010601     C                   exsr      opeini
008400010605     C                   exsr      rwvab
008500040302     C*
008600040302     C* Effettuo la chiamata al CLLE preposto
008700040302     C                   call(e)   'TITVVTC'
008800040302     C                   parm                    parccm
008900040302     C                   parm                    parmbr
009000040302     C                   parm      '2'           paropz
009100010601     C*
009200010601     C                   seton                                        lr
009300010601
009400010601
009500010601
009600010601
009700010601     C*--------------------------------------------------------
009800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009900010601     C*--------------------------------------------------------
010000010601     C     PREELA        BEGSR
010100010601     C*
010200010601     C* SE OCCORRE SPEDIRE IN FILIALE
010300010601     C                   if        invfil <> *zeros and
010400010601     C                             flgGiro = '0'
010500010601     C*
010600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010700010601     C                   eval      flgGiro = '1'
010800000724     C*
010900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011100010601     C     invfil        chain     azorg01l
011200000724     C                   if        %found
011300000724     C                   movel(p)  CMD(1)        depcmd
011400020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011500000724     C*
011600000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011700011113     C                   Z-ADD     150           LENGH            15 5
011800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011900000724     C                   PARM                    depcmd
012000000724     C                   PARM                    LENGH
012100000724     C*
012200000724     C                   endif
012300000616     C*
012400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012500000616     C                   movel(p)  CMD(2)        depcmd
012600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012700000616     C*
012800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012900011113     C                   Z-ADD     150           LENGH            15 5
013000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013100000616     C                   PARM                    depcmd
013200000616     C                   PARM                    LENGH
013300000613     C*
013400020604     C                   if        not %open(titv15ps)
013500020604     C                   open      titv15ps
013600000616     C                   except    testdett
013700000613     C                   endif
013800010601     C*
013900010601     C                   endif
014000010601     C*
014100010601     C                   ENDSR
014200010601     C***
014300010601
014400010601
014500010601
014600010601
014700010601
014800010601
014900010601
015000010601     C*--------------------------------------------------------
015100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015200010601     C*--------------------------------------------------------
015300010601     C     ENDELA        BEGSR
015400990915     C*
015500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015600000613     C*
015700020604     C                   if        %open(titv15ps)
015800000616     C                   except    findett
015900020604     C                   close     titv15ps
016000000613     C                   endif
016100000616     C*
016200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
016300011113     C                   Z-ADD     150           LENGH            15 5
016400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
016500000616     C                   PARM                    CMD(3)
016600000616     C                   PARM                    LENGH
016700041216     C*
016800041216     C* EFFETTUO CHIAMATA DI "CHIUSURA" AL *PGM ESTERNO CHE EFFETUA LA SCRITTTURA DEI VARI FILE ORM
016900041216     C   54              call(e)   'TITV84R'
017000041216     C                   parm      'C'           InTipl
017100041216     C                   parm                    tivlrds
017200041216     C                   parm                    wfivabds
017300000616     C*
017400010601     C                   ENDSR
017500010601     C***
017600010601
017700010601
017800010601
017900010601
018000000613
018100000613     C*--------------------------------------------------------
018200000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018300000613     C*--------------------------------------------------------
018400000621     C     STPR          BEGSR
018500000613     C*
018600020604     C                   if        not %open(titv15p)
018700020604     C                   open      titv15p
018800990915     C                   endif
018900990915     C*
019000990915     C                   except    riepilogo
019100990915     C*
019200020604     C                   if        %open(titv15p)
019300020604     C                   close     titv15p
019400990914     C                   endif
019500000613     C*
019600000613     C                   ENDSR
019700000613     C***
019800000613
019900010330
020000010601
020100010601
020200010601
020300010330     C*--------------------------------------------------------
020400021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
020500010330     C*--------------------------------------------------------
020600010605     C     RWVAB         BEGSR
020700010330     C*
020800010330     C                   if        not %open(tivin00r)
020900010330     C                   open      tivin00r
021000010330     C                   endif
021100021025     C                   if        not %open(fivabwwr)
021200021025     C                   open      fivabwwr
021300010330     C                   endif
021400040302     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
021500040302     C                   exsr      prevat
021600040302     C*
021700040302     C                   if        chkcall = '0'
021800040302     C*
021900040302     C                   if        not %open(fivatwwr)
022000040302     C                   open      fivatwwr
022100040302     C                   endif
022200010330     C*
022300040302     C                   clear                   §CTROKVB          7 0
022400040302     C                   clear                   §CTROKVT          7 0
022500040302     C                   clear                   §CTRMO            7 0
022600040302     C                   clear                   §CTRNO            7 0
022700010330     C*
022800010330     C                   DO        *HIVAL
022900010330     C*
023000010330     C                   READ      tivin00r                               70
023100020604     C                   EVAL      countrec = countrec + 1
023200020604     C*
023300020604     C* Escludo sempre il primo record xchè contiene le descrizioni dei campi
023400020604     C                   if        countrec > 1
023500010618     C*
023600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
023700010618     C                   if        vinflg = '1'
023800010618     C                   eval      flgOk = '1'
023900010618     C                   endif
024000010618     C*
024100010330     C                   if        vindta > *blanks
024200010330     C                   add       1             rrnum
024300010330     C*
024400010601     C                   if        *in70 = *off and
024500010330     C                             (vinflg = *blanks
024600010330     C                              or vinflg = '0'
024700010330     C                              or vinflg = '2')
024800010330     C*
024900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
025000010711     C                   if        vinflg = *blanks or vinflg = '0'
025100010711     C                   clear                   vinmsg
025200010711     C                   endif
025300010601     C*
025400010330     C                   exsr      inzvar
025500010330     C                   exsr      defcam
025600010605     C                   exsr      impvab
025700010601     C*
025800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025900010605     C                   eval      depfil = VABLNP
026000010601     C                   exsr      repfil
026100010601     C                   if        depfil = invfil
026200021025     C                   if        vlrpoi = 999
026300021025     C                   MOVE(P)   invfil        VABFGS
026400021025     C                   else
026500021025     C                   MOVE(P)   vlrpoi        VABFGS
026600021025     C                   endif
026700010601     C*
026800010601     C                   exsr      PREELA
026900010601     C*
027000010604     C* Ebbene...
027100010604     C*
027200040302     C  N31              ADD       1             §CTROKVB
027300040302     C   32              ADD       1             §CTRMO
027400040302     C   31              ADD       1             §CTRNO
027500021025     C  N31              WRITE     FIVAB000
027600040302     C  N31              exsr      wrivat
027700041216     C*
027800041216     C* Eseguo operazioni particolari x gestire ritiro contestuale alla consegna
027900041216     C                   if        *IN31 = *OFF AND
028000041216     C                             vabGMA = 'RC'
028100041216     C*
028200041216     C* Accendo 1 indicatore x condizioare la chiamata in chiusura del TITV84R
028300041216     C                   seton                                        54
028400041216     C*
028500041216     C* Effettuo chiamata a *pgm esterno che si occupa della scrittura dei vari file ORM
028600041216     C                   eval      wfivabds = fivabds
028700041216     C                   call      'TITV84R'
028800041216     C                   parm      *blanks       InTipl            1
028900041216     C                   parm                    tivlrds
029000041216     C                   parm                    wfivabds
029100041216     C*
029200041216     C                   endif
029300010604     C*
029400020604     C                   endif
029500020604     C*
029600010604     C                   if        *in31 = *off and
029700010604     C                             *in32 = *off
029800010604     C                   eval      vinflg = '1'
029900010604     C                   else
030000010604     C                   eval      recko = vindta
030100010604     C                   except    rigadett
030200010604     C                   eval      vinflg = '2'
030300010604     C                   endif
030400010604     C                   endif
030500010601     C*
030600010604     C                   endif
030700010604     C*
030800010330     C                   else
030900010330     C                   eval      vinflg = '1'
031000010330     C                   endif
031100010601     C*
031200010601     C  N70              update    tivin000
031300010330     C*
031400010330     C  N70              ENDdo
031500040302     C*
031600040302     C                   endif                                                  * endif chkcall
031700010601     C*
031800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
031900010601     C                   if        cntNonEl = *zeros or
032000010601     C                             flgMulti = '0'
032100010330     C* Se non ci sono record con errori ...
032200010601     C                   if        §ctrno = 0 and
032300010604     C                             §ctrmo = 0 and
032400010601     C                             flgStato <> '2'
032500010330     C* ... restituisco esito OK.
032600010330     C                   eval      wrkesito = '0'
032700010330     C                   else
032800040302     C                   if        §ctrokvb > 0
032900010330     C                   eval      wrkesito = '1'
033000010330     C                   else
033100010615     C                   if        flgOk = '0'
033200010615     C                   eval      wrkesito = '2'
033300010615     C                   else
033400010615     C                   eval      wrkesito = '6'
033500010615     C                   endif
033600010330     C                   endif
033700010330     C                   endif
033800010601     C                   else
033900010601     C                   eval      wrkesito = '9'
034000010601     C                   endif
034100010330     C*
034200010330     C                   if        %open(tivin00r)
034300010330     C                   close     tivin00r
034400010330     C                   endif
034500021025     C                   if        %open(fivabwwr)
034600021025     C                   close     fivabwwr
034700010330     C                   endif
034800040302     C                   if        %open(fivatwwr)
034900040302     C                   close     fivatwwr
035000040302     C                   endif
035100010601     C*
035200010601     C                   if        vlrpoi <> 999
035300010601     C                   eval      invfil = vlrpoi
035400010601     C                   endif
035500010330     C*
035600040302     C                   if        §ctrokvb > 0
035700010601     C                             and invfil > *zeros
035800010330     C                   exsr      invio
035900010330     C                   endif
036000010601     C*
036100010618     C                   if        flgGiro = '1'
036200010601     C                   exsr      endela
036300010618     C                   endif
036400010330     C*
036500010330     C                   ENDSR
036600010330     C***
036700010330
036800010601
036900010601
037000010601
037100010601
037200010330     C*----------------------------------------------------*
037300010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
037400010330     C*----------------------------------------------------*
037500010330     C     INZVAR        BEGSR
037600010330     C*
037700020204     C                   Z-ADD     *zeros        Num5_0            5 0
037800010330     C*
037900010330     C                   ENDSR
038000010330     C*----------------------------------------------------*
038100010330     C*  IMPOSTAZIONE CAMPI COSTANTI
038200010330     C*----------------------------------------------------*
038300010330     C     DEFCAM        BEGSR
038400010330     C*
038500020204     C* Inizializzo il buffer del record da scrivere
038600021025     C                   CLEAR                   FIVAB000
038700020204     C* Imposto i valori di default...
038800030506     C                   EVAL      VABCCM = 0632629
038900020604     C                   EVAL      VABLNP = 063
039000020204     C                   EVAL      VABCTR = 000
039100020204     C*
039200010330     C                   ENDSR
039300010607     C*----------------------------------------------------*
039400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
039500010607     C*----------------------------------------------------*
039600010607     C     IMPVAB        BEGSR
039700010607     C*
039800010607     C                   SETOFF                                       3132
039900010607     C                   MOVEL     datcor        VABAAS
040000010607     C                   MOVE      datcor        VABMGS
040100040302     C*
040200040302     C* Costruisco il numero spedizione
040300040302     C                   MOVE      vlrPRG        wNSPPRG1
040400040302     C                   ADD       1             wNSPPRG2
040500040302     C                   MOVEL     wNSPPRG1      vabNSP
040600040302     C                   MOVE      wNSPPRG2      vabNSP
040700010607     C*
040800010607     C* Reperimento campi ALFA
040900010607     C*
041000010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
041100020624     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
041200020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
041300020117     C     '@':'A'       XLATE     VABRSD        VABRSD
041400020117     C* ==
041500020624     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
041600020624     C                   EVAL      VABLOD=%trim(%subst(vindta:80:35))
041700020624     C                   EVAL      VABPRD=%trim(%subst(vindta:115:2))
041800020624     C                   EVAL      VABRMA=%trim(%subst(vindta:129:15))
041900020624     C                   EVAL      VABNOT=%trim(%subst(vindta:152:30))
042000041216     C                   EVAL      VABGMA=%trim(%subst(vindta:217:2))
042100041216     C*
042200041216     C* Se presente particolarità consegna 'RC' e se il codice
042300041216     C* cliente mittente è valorizzato e numero serie ORM nn passata
042400041216     C* nei parametri verifico se il cliente è abilitato in tabella 'OSR'
042500041216     C                   IF        VABGMA = 'RC'
042600041216     C                   IF        VABCCM > *ZEROS
042700041216     C                   Z-ADD     1             jOSR
042800080502     C                   move(p)   VABLNP        wLnpOSR
042900080502     C                   move(p)   VABCCM        wCliOSR
043000080502     C     DsCliOSR      LOOKUP    SkCliOSR(jOSR)                         55
043100041216     C                   IF        %found
043200041216     C                   EVAL      wNSR = %subst(SkLnpNrsOSR(jOSR):4:2)
043300041216     C                   EVAL      %subst(vlrppt:4:2) = wNSR
043400041216     C                   ELSE
043500041216     C                   SETON                                        31
043600041216     C                   EVAL      vinmsg = %trimr(vinmsg)
043700041216     C                             + ' ' + 'ORMNRS(tabellaOSR)'
043800041216     C                   ENDIF
043900041216     C                   ENDIF
044000041216     C                   ENDIF
044100010607     C*
044200010607     C* Reperimento campi NUMERICI
044300020604     C*
044400020604     C* CAP
044500020624     C                   EVAL      PiStr=%trim(%subst(vindta:71:9))
044600010607     C                   EXSR      CHKNUM
044700010607     C                   IF        PiInt=*on
044800020204     C                   Z-ADD     PiVal         Num5_0
044900020204     C                   MOVEL(p)  Num5_0        VABCAD
045000010607     C                   ELSE
045100010607     C                   SETON                                        32
045200020204     C                   EVAL      VABCAD = *zeros
045300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
045400020204     C                             + ' ' + 'VABCAD'
045500010607     C                   ENDIF
045600020604     C* Riferimento mittente numerico
045700020624     C                   EVAL      PiStr=%trim(%subst(vindta:129:15))
045800011023     C                   EXSR      CHKNUM
045900011023     C                   IF        PiInt=*on
046000011023     C                   Z-ADD     PiVal         VABRMN
046100011023     C                   ELSE
046200020205     C                   EVAL      VABRMN = 1
046300011023     C                   ENDIF
046400020604     C* Colli
046500020624     C                   EVAL      PiStr=%trim(%subst(vindta:117:5))
046600010607     C                   EXSR      CHKNUM
046700010607     C                   IF        PiInt=*on
046800010607     C                   Z-ADD     PiVal         VABNCL
046900010607     C                   ELSE
047000010607     C                   SETON                                        32
047100010607     C                   Z-ADD     *zeros        VABNCL
047200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
047300010607     C                             + ' ' + 'VABNCL'
047400010607     C                   ENDIF
047500020604     C* Peso
047600020624     C                   EVAL      PiStr=%trim(%subst(vindta:122:7))
047700010607     C                   EXSR      CHKNUM
047800010607     C                   IF        PiNum=*on
047900010607     C                   Z-ADD(H)  PiVal         VABPKB
048000010607     C                   ELSE
048100010607     C                   SETON                                        32
048200010607     C                   Z-ADD     *zeros        VABPKB
048300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
048400010607     C                             + ' ' + 'VABPKB'
048500010607     C                   ENDIF
048600020604     C* Data consegna richiesta
048700020624     C                   EVAL      PiStr=%trim(%subst(vindta:144:8))
048800020604     C                   EXSR      CHKNUM
048900020604     C                   IF        PiNum=*on
049000020604     C                   Z-ADD     PiVal         VABDCR
049100040621     C***                EVAL      VABTCR = 'P'
049200020604     C                   ENDIF
049300020204     C*
049400020204     C* Considerazioni finali sui valori sopra ricavati
049500020204     C                   IF        (VABCAS = *zeros)
049600020204     C                   EVAL      VABCBO = '1 '
049700020204     C                   ELSE
049800020204     C                   EVAL      VABCBO = '4 '
049900020204     C                   ENDIF
050000020204     C*
050100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
050200020204     C                   EXSR      CHKIMPDIV
050300020204     C*
050400010607     C                   ENDSR
050500010607     C*----------------------------------------------------*
050600040302
050700040302
050800040302
050900040302     C*----------------------------------------------------*
051000040302     C* SCRITTURA RECORD FIVAT
051100040302     C*----------------------------------------------------*
051200040302     C     WRIVAT        BEGSR
051300040302     C*
051400040302     C                   eval      vatFGS = vabFGS
051500040302     C                   eval      vatAAS = vabAAS
051600040302     C                   eval      vatCCM = vabCCM
051700040302     C                   eval      vatLNP = vabLNP
051800040302     C                   eval      vatNSP = vabNSP
051900040302     C                   eval      vatTRC = 'B'
052000040302     C                   eval      vatNOT = %subst(vindta:190:30)
052100040302     C                   write     FIVAT000
052200040302     C*
052300040302     C                   ENDSR
052400010601
052500020204
052600020204     C*----------------------------------------------------*
052700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052800020204     C*----------------------------------------------------*
052900020204     C     CHKIMPDIV     BEGSR
053000020204     C*
053100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053200020204     C                   Z-ADD     *zeros        wrkDec            9 9
053300020204     C*
053400020204     C* Come prima cosa effettuo considerazioni sulla divisa
053500020204     C                   IF        vabIAS > *zeros
053600020204     C                   IF        vabVAS <> 'EUR'
053700020204     C                   EVAL      vabVAS =  'ITL'
053800020204     C                   ENDIF
053900020204     C                   ENDIF
054000020204     C*
054100020204     C                   IF        vabCAS > *zeros
054200020204     C                   IF        vabVCA <> 'EUR'
054300020204     C                   EVAL      vabVCA =  'ITL'
054400020204     C                   ENDIF
054500020204     C                   ENDIF
054600020204     C*
054700020204     C                   IF        vabVMD > *zeros
054800020204     C                   IF        vabVAD <> 'EUR'
054900020204     C                   EVAL      vabVAD =  'ITL'
055000020204     C                   ENDIF
055100020204     C                   ENDIF
055200020204     C*
055300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055400020204     C                   Z-ADD     vabIAS        wrkDec
055500020204     C                   IF        wrkDec > *zeros
055600020204     C                   IF        vabVAS = 'ITL'
055700020204     C                   EVAL      vabIAS = *zeros
055800020204     C                   ENDIF
055900020204     C                   ENDIF
056000020204     C*
056100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
056200020204     C                   Z-ADD     vabCAS        wrkDec
056300020204     C                   IF        wrkDec > *zeros
056400020204     C                   IF        vabVCA = 'ITL'
056500020204     C                   EVAL      vabCAS = *zeros
056600020204     C                   ENDIF
056700020204     C                   ENDIF
056800020204     C*
056900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057000020204     C                   Z-ADD     vabVMD        wrkDec
057100020204     C                   IF        wrkDec > *zeros
057200020204     C                   IF        vabVAD = 'ITL'
057300020204     C                   EVAL      vabVMD = *zeros
057400020204     C                   ENDIF
057500020204     C                   ENDIF
057600020204     C*
057700020204     C                   ENDSR
057800020204     C***
057900040302
058000040302
058100040302
058200040302
058300040302     C*----------------------------------------------------*
058400040302     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
058500040302     C*----------------------------------------------------*
058600040302     C     PREVAT        BEGSR
058700040302     C*
058800040302     C* Compongo il nome del membro da dare al FIVATWWR
058900041217     C                   eval      parmbr = vlrhdl
059000041216     C                   move(P)   vlrksc        parccm
059100040302     C                   eval      paropz = '1'
059200040302     C* Effettuo la chiamata al CLLE preposto
059300040302     C                   call(e)   'TITVVTC'
059400040302     C                   parm                    parccm
059500040302     C                   parm                    parmbr
059600040302     C                   parm                    paropz
059700040302     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
059800040302     C                   if        %error
059900040302     C                   movel     '1'           chkcall
060000040302     C                   else
060100040302     C                   movel     '0'           chkcall
060200040302     C                   endif
060300040302     C*
060400040302     C                   ENDSR
060500020204
060600010330
060700010330
060800010330     C*----------------------------------------------------*
060900010330     C*  CONTROLLO NUMERICITA' CAMPI
061000010330     C*----------------------------------------------------*
061100010330     C     CHKNUM        BEGSR
061200010330     C*
061300010606     C                   IF        PiDecChr = *blanks
061400020624     C                   EVAL      PiDecChr = '.'
061500010606     C                   ENDIF
061600010606     C*
061700010606     C                   CALL(e)   'ISNUMERIC'
061800010330     C                   PARM                    PiStr            30
061900010606     C                   PARM                    PiDecChr          1
062000010330     C                   PARM      *ZEROS        PiVal            30 9
062100010330     C                   PARM      '0'           PiInt             1
062200010330     C                   PARM      '0'           PiNum             1
062300010330     C                   IF        %error
062400010606     C                   EVAL      PiNum=*off
062500010330     C                   ENDIF
062600010330     C*
062700010330     C                   ENDSR
062800010330     C***
062900010330
063000010601
063100010601
063200010601
063300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
063400010601     C     repfil        BEGSR
063500010601     C*
063600010601     C                   if        invfil = *zeros and
063700010601     C                             depfil > *zeros and
063800010629     C                             (vinflg = *blanks or
063900010629     C                              vinflg = *zeros)
064000010601     C
064100010601     C                   eval      invfil = depfil
064200010601     C                   endif
064300010601     C*
064400010601     C                   if        depfil <> invfil and
064500010601     C                             invfil > *zeros
064600010601     C                   eval      flgMulti = '1'
064700010601     C                   if        vinflg = *blanks
064800010601     C                   add       1             cntNonEl
064900010601     C                   endif
065000010601     C                   endif
065100010601     C*
065200010601     C                   if        vinflg = '2'
065300010601     C                   eval      flgStato = '2'
065400010601     C                   endif
065500010601     C*
065600010601     C                   ENDSR
065700010601     C***
065800010601
065900010601
066000010601
066100010330
066200010330
066300010330
066400990920      /TITLE Invio dei dati al punto operativo.
066500000613     C     invio         BEGSR
066600040302     C*
066700040302     C* 1° invio FIVAT
066800040302     C                   reset                   dscmz
066900041217     C                   move      invfil        cmzdst
067000040302     C                   eval      cmzfld = 'FIVATWWR'
067100040302     C                   eval      cmzmbd = vlrhdl
067200040302     C                   eval      %subst(cmzmbd:1:1) = 'M'
067300040302     C***                if        prmfir = *blanks
067400040302     C                   eval      cmzfla = 'FIVAT00F'
067500040302     C                   eval      cmzmba = 'FIVAT00F'
067600040302     C***                else
067700040302     C***                eval      cmzfla = prmfir
067800040302     C***                eval      cmzmba = prmfir
067900040302     C***                endif
068000040302     C                   eval      cmznrr = *zeros
068100040302     C                   move      §ctrokvt      cmznrr
068200040302     C                   eval      cmzlba = vlrfl1
068300040302     C                   call(e)   'TIS711C'
068400040302     C                   parm                    dscmz
068500040302     C                   parm      *blanks       esito
068600040302     C                   if        %error
068700040302     C                             or cmzerr = '1'
068800040302     C                             or esito  = '1'
068900040302     C                   eval      wrkesito = '3'
069000040302     C                   else
069100040302     C*
069200040302     C* 2° invio FIVAB
069300990920     C                   reset                   dscmz
069400010601     C                   move      invfil        cmzdst
069500990920     C                   eval      cmzfld = vlrfou
069600990920     C                   eval      cmzmbd = vlrhdl
069700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
069800040302     C***                if        prmfir = *blanks
069900021025     C                   eval      cmzfla = 'FIVAB00F'
070000021025     C                   eval      cmzmba = 'FIVAB00F'
070100040302     C***                else
070200040302     C***                eval      cmzfla = prmfir
070300040302     C***                eval      cmzmba = prmfir
070400040302     C***                endif
070500990920     C                   eval      cmznrr = *zeros
070600040302     C                   move      §ctrokvb      cmznrr
070700021018     C                   eval      cmzlba = vlrfl1
070800990920     C                   call(e)   'TIS711C'
070900990920     C                   parm                    dscmz
071000990921     C                   parm      *blanks       esito
071100990920     C                   if        %error
071200990920     C                             or cmzerr = '1'
071300990921     C                             or esito  = '1'
071400000710     C                   eval      wrkesito = '3'
071500990920     C                   endif
071600040302     C                   endif
071700990920     C*
071800000613     C                   ENDSR
071900990910
072000010601
072100010601
072200010601
072300010601
072400010601      /TITLE Invio dei dati al punto operativo.
072500010601     C     opeini        BEGSR
072600010601     C*
072700010601     C* Inizializzo flag e contatori operativi
072800010601     C                   movel     '0'           flgGiro           1
072900010601     C                   movel     '0'           flgMulti          1
073000010601     C                   movel     '1'           flgStato          1
073100010615     C                   movel     '0'           flgOk             1
073200010601     C                   z-add     *zeros        cntNonEl         10 0
073300010601     C                   z-add     *zeros        depfil            3 0
073400010601     C                   z-add     *zeros        invfil            3 0
073500010601     C*
073600010601     C                   ENDSR
073700010601     C***
073800010601
073900010601
074000041216
074100080502     C*--------------------------------------------------------
074200080502     C* CARTAB - CARICAMENTO TABELLE                          *
074300080502     C*--------------------------------------------------------
074400080502     C     CARTAB        BEGSR
074500080502     C*
074600080502     C* Carico tutta la tabella "OSR" delle LNP/NRS ORM
074700080502     C                   z-add     *zeros        jOSR              3 0
074800080502     C     'OSR'         setll     tntbe01l
074900080502     C                   if        %equal(tntbe01l)
075000080502     C     'OSR'         reade     tntbe01l
075100080502     C                   dow       not %eof(tntbe01l)
075200080502     C                   eval      DOSR = tbeUNI
075300080502     c*
075400080502     c* Verifica e forzatura x nuovi campi aggiunti
075500080502     c                   if        aD§OSRcl6 = *blanks
075600080502     c                   eval      D§OSRcl6 = *zeros
075700080502     c                   endif
075800080502     c                   if        aD§OSRcl7 = *blanks
075900080502     c                   eval      D§OSRcl7 = *zeros
076000080502     c                   endif
076100080502     c                   if        aD§OSRcl8 = *blanks
076200080502     c                   eval      D§OSRcl8 = *zeros
076300080502     c                   endif
076400080502     c                   if        aD§OSRcl9 = *blanks
076500080502     c                   eval      D§OSRcl9 = *zeros
076600080502     c                   endif
076700080502     c                   if        aD§OSRcl10 = *blanks
076800080502     c                   eval      D§OSRcl10 = *zeros
076900080502     c                   endif
077000080502     c                   if        aD§OSRcl11 = *blanks
077100080502     c                   eval      D§OSRcl11 = *zeros
077200080502     c                   endif
077300080502     c                   if        aD§OSRcl12 = *blanks
077400080502     c                   eval      D§OSRcl12 = *zeros
077500080502     c                   endif
077600080502     c                   if        aD§OSRcl13 = *blanks
077700080502     c                   eval      D§OSRcl13 = *zeros
077800080502     c                   endif
077900080502     c                   if        aD§OSRcl14 = *blanks
078000080502     c                   eval      D§OSRcl14 = *zeros
078100080502     c                   endif
078200080502     c                   if        aD§OSRcl15 = *blanks
078300080502     c                   eval      D§OSRcl15 = *zeros
078400080502     c                   endif
078500080502     c                   if        aD§OSRcl16 = *blanks
078600080502     c                   eval      D§OSRcl16 = *zeros
078700080502     c                   endif
078800080502     c*
078900080502     C* Verifico tutti i 16 codici clienti che è possibile indicare x ogni serie
079000080502     C                   if        D§OSRCLI <> *zeros
079100080502     C                   add       1             jOSR
079200080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
079300080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
079400080502     C                   movel     D§OSRCLI      wCliOSR
079500080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
079600080502     C                   endif
079700080502     C*
079800080502     C                   if        D§OSRCL2 <> *zeros
079900080502     C                   add       1             jOSR
080000080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
080100080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
080200080502     C                   movel     D§OSRCL2      wCliOSR
080300080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
080400080502     C                   endif
080500080502     C*
080600080502     C                   if        D§OSRCL3 <> *zeros
080700080502     C                   add       1             jOSR
080800080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
080900080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
081000080502     C                   movel     D§OSRCL3      wCliOSR
081100080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
081200080502     C                   endif
081300080502     C*
081400080502     C                   if        D§OSRCL4 <> *zeros
081500080502     C                   add       1             jOSR
081600080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
081700080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
081800080502     C                   movel     D§OSRCL4      wCliOSR
081900080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
082000080502     C                   endif
082100080502     C*
082200080502     C                   if        D§OSRCL5 <> *zeros
082300080502     C                   add       1             jOSR
082400080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
082500080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
082600080502     C                   movel     D§OSRCL5      wCliOSR
082700080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
082800080502     C                   endif
082900080502     C*
083000080502     C                   if        D§OSRCL6 <> *zeros
083100080502     C                   add       1             jOSR
083200080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
083300080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
083400080502     C                   movel     D§OSRCL6      wCliOSR
083500080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
083600080502     C                   endif
083700080502     C*
083800080502     C                   if        D§OSRCL7 <> *zeros
083900080502     C                   add       1             jOSR
084000080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
084100080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
084200080502     C                   movel     D§OSRCL7      wCliOSR
084300080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
084400080502     C                   endif
084500080502     C*
084600080502     C                   if        D§OSRCL8 <> *zeros
084700080502     C                   add       1             jOSR
084800080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
084900080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
085000080502     C                   movel     D§OSRCL8      wCliOSR
085100080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
085200080502     C                   endif
085300080502     C*
085400080502     C                   if        D§OSRCL9 <> *zeros
085500080502     C                   add       1             jOSR
085600080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
085700080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
085800080502     C                   movel     D§OSRCL9      wCliOSR
085900080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
086000080502     C                   endif
086100080502     C*
086200080502     C                   if        D§OSRCL10 <> *zeros
086300080502     C                   add       1             jOSR
086400080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
086500080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
086600080502     C                   movel     D§OSRCL10     wCliOSR
086700080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
086800080502     C                   endif
086900080502     C*
087000080502     C                   if        D§OSRCL11 <> *zeros
087100080502     C                   add       1             jOSR
087200080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
087300080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
087400080502     C                   movel     D§OSRCL11     wCliOSR
087500080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
087600080502     C                   endif
087700080502     C*
087800080502     C                   if        D§OSRCL12 <> *zeros
087900080502     C                   add       1             jOSR
088000080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
088100080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
088200080502     C                   movel     D§OSRCL12     wCliOSR
088300080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
088400080502     C                   endif
088500080502     C*
088600080502     C                   if        D§OSRCL13 <> *zeros
088700080502     C                   add       1             jOSR
088800080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
088900080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
089000080502     C                   movel     D§OSRCL13     wCliOSR
089100080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
089200080502     C                   endif
089300080502     C*
089400080502     C                   if        D§OSRCL14 <> *zeros
089500080502     C                   add       1             jOSR
089600080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
089700080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
089800080502     C                   movel     D§OSRCL14     wCliOSR
089900080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
090000080502     C                   endif
090100080502     C*
090200080502     C                   if        D§OSRCL15 <> *zeros
090300080502     C                   add       1             jOSR
090400080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
090500080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
090600080502     C                   movel     D§OSRCL15     wCliOSR
090700080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
090800080502     C                   endif
090900080502     C*
091000080502     C                   if        D§OSRCL16 <> *zeros
091100080502     C                   add       1             jOSR
091200080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
091300080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
091400080502     C                   movel     D§OSRCL16     wCliOSR
091500080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
091600080502     C                   endif
091700080502     C*
091800080502     C     'OSR'         reade     tntbe01l
091900080502     C                   enddo
092000080502     C                   endif
092100080502     C*
092200080502     C                   ENDSR
092300080502     C***
092400010601
092500010330
092600010330
092700000613     C     *inzsr        BEGSR
092800990910     C*
092900990910     C     *entry        plist
093000990920     C                   parm                    tivlrds
093100990921     C                   parm      wrkesito      esito
093200000724     C                   parm                    prmlit
093300000710     C                   parm                    prmfir
093400041216     C*
093500041216     C* CARICO LE TABELLE OCCORRENTI
093600041216     C                   exsr      CARTAB
093700010330     C*
093800010330     C* CALCOLA LA DATA CORRENTE
093900010330     C                   time                    wn14             14 0
094000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
094100010330     C                   z-add     wn8           g08dat
094200010330     C                   z-add     *zeros        g08inv
094300010330     C                   movel     '0'           g08err
094400010330     C                   call      'XSRDA8'
094500010330     C                   parm                    wlbda8
094600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
094700000613     C*
094800000613     C                   ENDSR
094900000613     C***
095000990908
095100020604     Otitv15p   E            riepilogo         2
095200990915     O                                              'Upload via Internet'
095300990915     O                                           +1 'Traduzione TIVIN00R -'
095400021025     O                                           +1 'FIVABWWR'
095500990915     O                                           +1 'Testate bolle'
095600990915     O          E            riepilogo   2
095700990915     O                       wrkdata
095800990915     O                       wrkora              +1
095900990915     O                       procname            +1
096000990915     O          E            riepilogo   2
096100990915     O                                              'Cliente..................:'
096200010605     O                       VABCCM        z     +1
096300990915     O          E            riepilogo   2
096400990920     O                                              'Riferimento Strategi.....:'
096500990920     O                       vlrhdl              +1
096600990915     O          E            riepilogo   2
096700990915     O                                              'Giusti...................:'
096800040302     O                       §CTROKVB      2   +  1
096900990915     O          E            riepilogo   2
097000990915     O                                              'Sbagliati e corretti.....:'
097100971022     O                       §CTRMO        2   +  1
097200990915     O          E            riepilogo   2
097300990915     O                                              'Sbagliati e scartati.....:'
097400971022     O                       §CTRNO        2   +  1
097500000613
097600020604     Otitv15ps  E            testdett          2
097700000613     O                                              'Upload via Internet'
097800000613     O                                           +1 'Traduzione TIVIN00R -'
097900021025     O                                           +1 'FIVABWWR'
098000000613     O                                           +1 'Testate bolle'
098100000616     O          E            testdett    3
098200000613     O                                           +2 'N° rec'
098300000613     O                                           +3 'Anteprima contenuto'
098400000616     O          E            rigadett    2
098500000613     O                       rrnum               +2
098600000621     O                       recko               +3
098700000616     O          E            findett     2
098800000613     O                       wrkdata
098900000613     O                       wrkora              +1
099000000613     O                       procname            +1
099100000616     O          E            findett     2
099200000613     O                                              'Cliente..................:'
099300010605     O                       VABCCM        z     +1
099400000616     O          E            findett     2
099500000613     O                                              'Riferimento Strategi.....:'
099600000613     O                       vlrhdl              +1
099700000616     O          E            findett     2
099800000613     O                                              'Giusti...................:'
099900040302     O                       §CTROKVB      2   +  1
100000000616     O          E            findett     2
100100000613     O                                              'Sbagliati e corretti.....:'
100200000613     O                       §CTRMO        2   +  1
100300000616     O          E            findett     2
100400000613     O                                              'Sbagliati e scartati.....:'
100500000613     O                       §CTRNO        2   +  1
100600000613** CMD - COMANDI CL
100700020604OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV15P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
100800020604OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV15PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
100900020604DLTOVR FILE(TITV15P TITV15PS) LVL(*)
101000010606
101100000613
