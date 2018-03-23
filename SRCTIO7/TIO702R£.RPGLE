000100060609     h DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('TIS')
000200000000     hDATEDIT(*DMY)
000300000000     h*--------------------------------------------------------------------------------------------*
000400000000     h* Description: ORM Immissione - Controlli parte 1
000500000000     h* Richiamato dal TIO101S su AS777 con OPCODE = CHKPARTE1
000600000000     f*--------------------------------------------------------------------------------------------*
000700000000     f* Data base
000800000000     f*--------------------------------------------------------------------------------------------*
000900000000     h*---
001000000000     h* Clienti abilitati
001100000000     h*---
001200000000     ftivss02l  IF   E           K DISK    USROPN
001300000000     h*---
001400000000     h* Clienti ritiro
001500000000     h*---
001600000000     ffnacr01l  IF   E           K DISK    USROPN
001700000000     f*---
001800000000     f* Tabelle
001900000000     f*---
002000000000     ftabel00f  IF   E           K DISK    USROPN
002100000000     f*---
002200000000     f* Tabelle -NEW-
002300000000     f*---
002400000000     ftntbe01l  IF   E           K DISK    USROPN
002500000000     ftntbe02l  IF   E           K DISK    USROPN
002600000000     f                                     RENAME(tntbe000:tntbe2)
002700000000     f*---
002800000000     f* Organigramma
002900000000     f*---
003000000000     fazorg01l  IF   E           K DISK    USROPN
003100050622
003200050622     D xcdfis          PR
003300050622     D                                     EXTPGM('XCDFIS')
003400050622     D                               16
003500050622     D                                1  0
003600060609     D rtvMsgLang      PR          3512A                                        Messaggio in lingua
003700060612     D  msgId                         7A   CONST
003800060612     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
003900060613     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
004000060613     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
004100060612     D                                     VARYING
004200060612     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
004300060612     D                                     VARYING
004400060612     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
004500060612     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
004600060612
004700000000     d*--------------------------------------------------------------------------------------------*
004800000000     d* Data structure
004900000000     d*--------------------------------------------------------------------------------------------*
005000000000     d*---
005100000000     d* Costanti
005200000000     d*---
005300000000     d minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005400000000     d maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005500000000     d $Dim            C                   CONST(200)                           *n° clienti da memo
005600120829     D IRLANDA         C                   'IRL'
005700120829     D EIRE            C                   'EIRE'
005800000000     d*---
005900000000     d* Schiere a tempo di compilazione
006000000000     d*---
006100000000     d msg             S             80    DIM(20) CTDATA PERRCD(1)             *messaggi di uscita
006200000000     d*---
006300000000     d* Variabili riferite al data base
006400000000     d*---
006500000000     d kblkut          S                   LIKE(tblkut)                         *lettura tabel00f
006600000000     d kblcod          S                   LIKE(tblcod)
006700000000     d kbecod          S                   LIKE(tbecod)                         *lettura tntbe01l
006800000000     d kbeke1          S                   LIKE(tbeke1)
006900000000     d kbelin          S                   LIKE(tbelin)
007000000000     d kbesif          S                   LIKE(tbesif)
007100000000     d kcrcro          S                   LIKE(acrcro)                         *lettura fnacr00f
007200000000     d kssksu          S                   LIKE(vssksu)                         *lettura tivss00f
007300000000     d kssisv          S                   LIKE(vssisv)
007400000000     d wacr            S                   LIKE(acrcro)                         *di lavoro
007500000000     d deplna          S                   LIKE(o95lna)                         *depositi
007600000000     d NInpo102Cfo     S                   LIKE(Inpo102Cfo)                     *campi"normalizzati"
007700000000     d NInpo102Pio     S                   LIKE(Inpo102Pio)
007800000000     d NInpo102Cro     S                   LIKE(acrcro)
007900000000     d NInpo102Rsr     S                   LIKE(Inpo102Rsr)
008000000000     d NInpo102Inr     S                   LIKE(Inpo102Inr)
008100000000     d NInpo102Car     S                   LIKE(Inpo102Car)
008200000000     d NInpo102Lor     S                   LIKE(Inpo102Lor)
008300000000     d NInpo102Prr     S                   LIKE(Inpo102Prr)
008400000000     d NInpo102Nar     S                   LIKE(Inpo102Nar)
008500000000     d NInpo102Pag     S                   LIKE(Inpo102Pag)
008600000000     d*---
008700000000     d* Variabili di controlli
008800000000     d*---
008900000000     d $e              S              2  0                                      *indice errori Out
009000000000     d $err            S              1                                         *controllore errore
009100000000     d $OkCro          S              1                                         *validità C.Ritiro
009200000000     d*---
009300000000     d* Variabili NON riferite al data base
009400000000     d*---
009500000000     d i               S              3  0                                      *indici
009600000000     d ii              S              3  0
009700000000     d n14             S             14  0                                      *Numerico 14
009800000000     d n8              S              8  0                                      *Numerico 8
009900000000     d n3              S              3  0                                      *Numerico 3
010000000000     d a1              S              1                                         *Alfabetico 1
010100000000     d a3              S              3                                         *Alfabetico 3
010200000000     d datcor          S              8  0                                      *data corrente a/m/g
010300000000     d oracor          S              6  0                                      *ora corrente
010400000000     d depcli          S              7  0                                      *depositi
010500000000     d werr            S              1                                         *errore generico
010600000000     d wcli            S              7  0                                      *di lavoro
010700000000     d wpgm            S             20                                         *programma
010800000000     d wdat            S              8  0                                      *data (aaaammgg)
010900000000     d wdate           S             10                                         *data (gg/mm/aaaa)
011000000000     d wora            S              4  0                                      *ora  (hhmm)
011100000000     d worae           S              5                                         *ora  (hh:mm)
011200050622     D xcdfis16        S             16
011300050622     d xstato          S              1  0
011400000000     d*---
011500000000     d* Schiere
011600000000     d*---
011700000000     d SErr            S              1    DIM(10)                              *errori          Out
011800000000     d SMsg            S             80    DIM(10)                              *messaggi errori Out
011900000000      * clienti figli
012000000000     d sksc7           S              7  0 DIM(500)                             *clienti figli
012100000000      * clienti ritiro
012200000000     d SCro            S             10S 0 Dim($Dim)                            *clienti ritiro
012300000000      * tabella filiali
012400000000     d SOrg            S              3  0 DIM(300)                             *filiale
012500000000     d SOdes           S             30    DIM(300)                             *descrizione filiale
012600000000     d SOfl1           S              1    DIM(300)                             *flag italia/estero
012700020702     d SOpt            S              3    DIM(300)                             *flag poste/no poste
012800020702     d SOdpd           S              3    DIM(300)                             *flag DPD/no DPD
012900000000      * tabella provincie
013000000000     d SPrv            S              2    DIM(200)                             *provincia
013100000000     d SPDes           S             20    DIM(200)                             *descriz.provincia
013200000000      * tabella nazioni
013300011218     d SNaz            S              3    DIM(500)                             *nazione
013400011218     d SNDes           S             25    DIM(500)                             *descrizione nazione
013500000000     d*---
013600000000     d* Parametri I/O
013700000000     d*---
013800000000     d Tio102IDs     E Ds                  PREFIX(Inp)                          *Input
013900000000     d Tio102ODs     E Ds                  PREFIX(Out)                          *Output
014000000000     d*---
014100000000     d* Ds
014200000000     d*---
014300000000     d* controllo data
014400000000     d wlbda8          DS                  INZ                                  *controlla data
014500000000     d  g08dat                        8  0                                       -data dritta
014600000000     d  g08inv                        8  0                                       -data invertita
014700000000     d  g08err                        1                                          -errore
014800000000     d  g08tgi                        5  0                                       -giorni fra date
014900000000      * scomposizione data generica
015000000000     d dsdat           DS                  INZ                                  *data
015100000000     d  dsann                         4  0                                       -anno
015200000000     d  dsmes                         2  0                                       -mese
015300000000     d  dsgio                         2  0                                       -giorno
015400000000      * scomposizione data generica (anno + mese/giorno)
015500000000     d                 DS                  INZ                                  *data aa + mm/gg
015600000000     d  dsannB                 1      4  0                                       -anno
015700000000     d  dsmgsB                 5      8  0                                       -mese/giorno
015800000000     d dsdatB                  1      8  0                                      *data aa + mm/gg
015900000000      * composizione data generica editata
016000000000     d dsdate          DS                  INZ                                  *data
016100000000     d  dsgioe                        2  0                                       -giorno
016200000000     d  dsvd1                         1                                          -'/'
016300000000     d  dsmese                        2  0                                       -mese
016400000000     d  dsvd2                         1                                          -'/'
016500000000     d  dsanne                        4  0                                       -anno
016600000000      * scomposizione ora generica
016700000000     d dsora           DS                  INZ                                  *ora
016800000000     d  dshh                          2  0                                       -ore
016900000000     d  dsmm                          2  0                                       -minuti
017000000000      * composizione ora generica editata
017100000000     d dsorae          DS                  INZ                                  *ora
017200000000     d  dshhe                         2  0                                       -ore
017300000000     d  dsvo1                         1                                          -':'
017400000000     d  dsmme                         2  0                                       -minuti
017500000000     d* controlla cap
017600000000     d tisi95ds      E DS                                                       *parametri chiamata
017700000000     d* reperimento clienti da cliente unificante
017800000000     d tibs10ds      E DS                                                       *parametri chiamata
017900000000     d  sksc11                21   5520  0 DIM(500)                              -schiera clienti
018000000000      * lettura campo orgde3 di AZORG00F
018100000000     d og143         E DS
018200000000      * DS Provincie
018300000000     d dspr          E DS
018400000000      * DS Nazioni
018500000000     d ds15          E DS
018600000000      * Ds ricerca cliente P.d.C.
018700000000     d bs69ds        E DS                  EXTNAME(tibs69ds) INZ
018800000000     d acods         E DS                  EXTNAME(cnaco00f) INZ
018900000000     d indds         E DS                  EXTNAME(cnind00f) INZ
019000000000     d clpds         E DS                  EXTNAME(cnclp00f) INZ
019100000000     d clsds         E DS                  EXTNAME(fncls00f) INZ
019200000000      * conrollo partita IVA italiana
019300000000     d xivaeu        E DS
019400020702      * DS lettura tabella "NTW"
019500020702     d DNTW          E DS
019600060601
019700000000     c*--------------------------------------------------------------------------------------------*
019800000000     c* Main lines
019900000000     c*--------------------------------------------------------------------------------------------*
020000000000     c*
020100000000     c* operazioni iniziali -da fare sempre-
020200000000     c                   EXSR      rutinz
020300000000     c*
020400000000     C* memorizza in schiere i clienti ritiro dell'utente STRATEGI
020500000000if  1c                   IF        InpO102Ksu > *zeros                          *cliente PASSWORD
020600000000     c                   EXSR      CarAcr
020700000000e   1c                   ENDIF
020800000000     c*
020900000000     c* cliente SENZA PW --> controlla il codice fiscale o la partita Iva ordinante
021000000000if  1c                   IF        InpO102Ksu > *zeros
021100000000x   1c                   ELSE                                                   *cliente SENZA PW
021200000000     c                   EXSR      ChkInpCfo
021300000000     c                   EXSR      ChkInpPio
021400000000     c                   EXSR      ChkInpCpfPio
021500000000e   1c                   ENDIF
021600000000     c*
021700000000     c* controlla il codice cliente ritiro
021800000000     c                   EXSR      ChkInpCro
021900000000     c*
022000000000     c* controlla il nominativo ritiro
022100000000     c                   EXSR      ChkInpRsr
022200000000     c*
022300000000     c* controlla l' indirizzo ritiro
022400000000     c                   EXSR      ChkInpInr
022500000000     c*
022600000000     c* controlla la nazione ritiro
022700000000     c                   EXSR      ChkInpNar
022800000000     c*
022900000000     c* controlla il CAP ritiro
023000000000     c                   EXSR      ChkInpCar
023100000000     c*
023200000000     c* controlla la località ritiro
023300000000     c                   EXSR      ChkInpLor
023400000000     c*
023500000000     c* controlla la provincia ritiro
023600000000     c                   EXSR      ChkInpPrr
023700000000     c*
023800000000     c* controlla l' indirizzo + il CAP + la località + la provincia + la nazione ritiro
023900000000     c                   EXSR      ChkInpICKPNr
024000000000     c*
024100000000     c* controlla il pagamento
024200000000     c                   EXSR      ChkInpPag
024300000000     c*
024400000000     c* imposta i campi della DS di Output O eventuali errori
024500000000if  1c                   IF        $err = '0'                                   *NO errori
024600000000     c                   EXSR      ImpDsOutB
024700000000X   1C                   ELSE                                                   *SI errori
024800000000     c                   MOVEA     SErr          OutO102Err                     *codice
024900000000     c                   MOVEA     SMsg          OutO102Msg                     *messaggio
025000000000e   1c                   ENDIF
025100000000     c*
025200000000     C                   RETURN
025300000000     c*--------------------------------------------------------------------------------------------*
025400000000     c* controlla il codice fiscale ordinante
025500000000     c*--------------------------------------------------------------------------------------------*
025600000000     c     ChkInpCfo     BEGSR
025700000000     c*
025800000000     c* reimposta le variabili di lavoro
025900000000     c                   MOVEL     '0'           werr                           *NO errore
026000000000     c                   CLEAR                   NInpo102Cfo                    *campo"normalizzato"
026100000000     c*
026200000000     c* controlla il dato solo se immesso
026300000000do  1c                   DO
026400000000if  2c                   IF        Inpo102Cfo <> *blanks                        *se immesso
026500000000     c*
026600000000     c* trasforma il campo immesso da minuscolo a maiuscolo
026700000000     c     minu:maiu     XLATE     Inpo102Cfo    NInpo102Cfo                    *Minus -> Maiuscolo
026800050622     C                   EVAL      xcdfis16 = ninpo102cfo
026900050622     c                   CALLP     xcdfis(xcdfis16:xstato)
027000050622     c                   IF        xstato < 0
027100050622     c                   EVAL      werr = *ON
027200050622     c                   LEAVE
027300050622     c                   ENDIF
027400000000     c*
027500000000     c* il 1° , 2° , 3° carattere devono essere lettere maiuscole -cognome-
027600050622if  3c*                  IF        %SUBST(NInpo102Cfo:1:3) =  *blanks OR
027700050622     c*                            %SUBST(NInpo102Cfo:1:3) >= *zeros
027800050622     c*                  MOVEL     '1'           werr                           *SI errore
027900050622     c*                  LEAVE
028000050622e   3c*                  ENDIF
028100000000     c*
028200000000     c* il 4° , 5° , 6° carattere devono essere lettere maiuscole -nome-
028300050622if  3c*                  IF        %SUBST(NInpo102Cfo:4:3) =  *blanks OR
028400050622     c*                            %SUBST(NInpo102Cfo:4:3) >= *zeros
028500050622     c*                  MOVEL     '1'           werr                           *SI errore
028600050622     c*                  LEAVE
028700050622e   3c*                  ENDIF
028800000000     c*
028900000000     c* il 7° , 8° carattere devono essere numeri -anno di nascita-
029000050622if  3c*                  IF        %SUBST(NInpo102Cfo:7:2) >= *zeros
029100050622x   3c*                  ELSE
029200050622     c*                  MOVEL     '1'           werr                           *SI errore
029300050622     c*                  LEAVE
029400050622e   3c*                  ENDIF
029500000000     c*
029600000000     c* il 9° carattere deve essere una lettera fra quelle possibili -mese di nascita-
029700050622if  3c*                  IF        %SUBST(NInpo102Cfo:9:1)= 'A' OR
029800050622     c*                            %SUBST(NInpo102Cfo:9:1)= 'B' OR
029900050622     c*                            %SUBST(NInpo102Cfo:9:1)= 'C' OR
030000050622     c*                            %SUBST(NInpo102Cfo:9:1)= 'D' OR
030100050622     c*                            %SUBST(NInpo102Cfo:9:1)= 'E' OR
030200050622     c*                            %SUBST(NInpo102Cfo:9:1)= 'H' OR
030300050622     c*                            %SUBST(NInpo102Cfo:9:1)= 'L' OR
030400050622     c*                            %SUBST(NInpo102Cfo:9:1)= 'M' OR
030500050622     c*                            %SUBST(NInpo102Cfo:9:1)= 'P' OR
030600050622     c*                            %SUBST(NInpo102Cfo:9:1)= 'R' OR
030700050622     c*                            %SUBST(NInpo102Cfo:9:1)= 'S' OR
030800050622     c*                            %SUBST(NInpo102Cfo:9:1)= 'T'
030900050622x   3c*                  ELSE
031000050622     c*                  MOVEL     '1'           werr                           *SI errore
031100050622     c*                  LEAVE
031200050622e   3c*                  ENDIF
031300000000     c*
031400000000     c* il 10°, 11° carattere devono essere numeri -giorno di nascita-
031500050622if  3c*                  IF        %SUBST(NInpo102Cfo:10:2) > *zeros
031600050622x   3c*                  ELSE
031700050622     c*                  MOVEL     '1'           werr                           *SI errore
031800050622     c*                  LEAVE
031900050622e   3c*                  ENDIF
032000000000     c*
032100000000     c* il 12°, 13°, 14°, 15° carattere devono essere 1 lettera + 3 numeri -codice comune nascita-
032200050622if  3c*                  IF        %SUBST(NInpo102Cfo:12:1) = *blanks  OR
032300050622     c*                            %SUBST(NInpo102Cfo:12:1) >= *zeros
032400050622     c*                  MOVEL     '1'           werr                           *SI errore
032500050622     c*                  LEAVE
032600050622e   3c*                  ENDIF
032700050622if  3c*                  IF        %SUBST(NInpo102Cfo:13:3) < *zeros
032800050622     c*                  MOVEL     '1'           werr                           *SI errore
032900050622     c*                  LEAVE
033000050622e   3c*                  ENDIF
033100050622e   2c                   ENDIF
033200000000     C*
033300000000e   1c                   ENDDO
033400000000     c*
033500000000     c* se errore, memorizza il codice errore nella schiera errori
033600000000if  1c                   IF        werr = '1'
033700000000if  2c                   IF        $e < 10                                      *non + di 10 errori
033800000000     c                   ADD       1             $e
033900000000     c                   MOVEL     '1'           Serr($e)                       *aggiunge errore
034000060601     c*                  MOVEL     msg(1)        Smsg($e)                       *messaggio
034100060609     c                   EVAL      Smsg($e) = rtvMsgLang('TIS0069'
034200060609     C                             :inpO102Lang)
034300000000     c                   MOVEL     '1'           $err                           *controllore errore
034400000000e   2c                   ENDIF
034500000000e   1c                   ENDIF
034600000000     c*
034700000000     c                   ENDSR
034800000000     c*--------------------------------------------------------------------------------------------*
034900000000     c* controlla la partita Iva ordinante
035000000000     c*--------------------------------------------------------------------------------------------*
035100000000     c     ChkInpPio     BEGSR
035200000000     c*
035300000000     c* reimposta le variabili di lavoro
035400000000     c                   MOVEL     '0'           werr                           *NO errore
035500000000     c                   CLEAR                   NInpo102Pio                    *campo"normalizzato"
035600000000     c*
035700000000     c* controlla il dato solo se immesso
035800000000do  1c                   DO
035900000000if  2c                   IF        Inpo102Pio <> *blanks                        *se immesso
036000000000     c*
036100000000     c* trasforma il campo immesso da minuscolo a maiuscolo
036200000000     c     minu:maiu     XLATE     Inpo102Pio    NInpo102Pio                    *Minus -> Maiuscolo
036300000000     c*
036400000000     c* conrolla validità formale
036500000000     c                   CLEAR                   XIvaEu
036600000000     c                   MOVEL     *blanks       IvaNar
036700000000     c                   MOVEL     NInpo102Pio   Ivapar
036800000000     c                   CALL      'XIVAEUR'
036900000000     c                   PARM                    XIvaEu
037000000000if  3c                   IF        ivaflg < 0
037100000000     c                   MOVEL     '1'           werr                           *SI errore
037200000000     c                   LEAVE
037300000000e   3c                   ENDIF
037400000000e   2c                   ENDIF
037500000000     C*
037600000000e   1c                   ENDDO
037700000000     c*
037800000000     c* se errore, memorizza il codice errore nella schiera errori
037900000000if  1c                   IF        werr = '1'
038000000000if  2c                   IF        $e < 10                                      *non + di 10 errori
038100000000     c                   ADD       1             $e
038200000000     c                   MOVEL     '2'           Serr($e)                       *aggiunge errore
038300060601     c*                  MOVEL     msg(2)        Smsg($e)                       *messaggio
038400060609     c                   EVAL      Smsg($e) = rtvMsgLang('TIS0444'
038500060609     C                             :inpO102Lang)
038600000000     c                   MOVEL     '1'           $err                           *controllore errore
038700000000e   2c                   ENDIF
038800000000e   1c                   ENDIF
038900000000     c*
039000000000     c                   ENDSR
039100000000     c*--------------------------------------------------------------------------------------------*
039200000000     c* controlla il codice fiscel e la partita Iva ordinante
039300000000     c*--------------------------------------------------------------------------------------------*
039400000000     c     ChkInpCpfPio  BEGSR
039500000000     c*
039600000000     c* reimposta le variabili di lavoro
039700000000     c                   MOVEL     '0'           werr                           *NO errore
039800000000     c*
039900000000do  1c                   DO
040000000000     c*
040100000000     C* solo uno dei due, codice fiscale o partita iva, deve essere pieno
040200000000if  2c                   IF        Inpo102Pio <> *blanks  AND
040300000000     c                             Inpo102Cfo <> *blanks
040400000000     c                   MOVEL     '1'           werr                           *SI errore
040500000000e   2c                   ENDIF
040600000000     c*
040700000000     C* almeno uno dei due, codice fiscale o partita iva, deve essere pieno
040800000000if  2c                   IF        Inpo102Pio = *blanks  AND
040900000000     c                             Inpo102Cfo = *blanks
041000000000     c                   MOVEL     '1'           werr                           *SI errore
041100000000e   2c                   ENDIF
041200000000     C*
041300000000e   1c                   ENDDO
041400000000     c*
041500000000     c* se errore, memorizza il codice errore nella schiera errori
041600000000if  1c                   IF        werr = '1'
041700000000if  2c                   IF        $e < 10                                      *non + di 10 errori
041800000000     c                   ADD       1             $e
041900000000     c                   MOVEL     'C'           Serr($e)                       *aggiunge errore
042000060601     c*                  MOVEL     msg(12)       Smsg($e)                       *messaggio
042100060609     c                   EVAL      Smsg($e) = rtvMsgLang('TIS0240'
042200060609     C                             :inpO102Lang)
042300000000     c                   MOVEL     '1'           $err                           *controllore errore
042400000000e   2c                   ENDIF
042500000000e   1c                   ENDIF
042600000000     c*
042700000000     c                   ENDSR
042800000000     c*--------------------------------------------------------------------------------------------*
042900000000     c* controlla il codice cliente ritiro
043000000000     c*--------------------------------------------------------------------------------------------*
043100000000     c     ChkInpCro     BEGSR
043200000000     c*
043300000000     c* reimposta le variabili di lavoro
043400000000     c                   MOVEL     '0'           werr                           *NO errore
043500000000     c                   CLEAR                   NInpO102Cro                    *campo"normalizzato"
043600000000     c                   MOVEL     'N'           $OkCro                         *validità
043700000000     c*
043800000000     c* controlla il dato solo se immesso
043900000000do  1c                   DO
044000000000if  2c                   IF        InpO102Cro <> *blanks AND
044100050207     c                             InpO102Cro <> *zeros
044200000000     c*
044300000000     c* controlla se il codice cliente ritiro è tutto un numero
044400000000     c                   SETOFF                                       98
044500000000     c                   TESTN                   InpO102Cro           98
044600000000if  3c                   IF        *in98                                        *da TESTN è numero
044700000000     c                   MOVE      InpO102Cro    a1                             *ultimo carattere
044800000000if  4c                   IF        a1 >= '0'                                    *NON trovata lettera
044900000000     c                   MOVEL     'S'           $OkCro                         *valido
045000000000e   4c                   ENDIF
045100000000e   3c                   ENDIF
045200000000if  3c                   IF        $OkCro = 'N'                                 *NON valido
045300000000     c                   MOVEL     '1'           werr                           *errore Output
045400000000     c                   LEAVE
045500000000e   3c                   ENDIF
045600000000     c*
045700000000     c* controlla se il cliente ritiro appartiene a quelli del codice STRATEGI di entrata
045800000000if  3c                   IF        werr = '0'                                   *ancora valido
045900000000     c                   MOVEL     InpO102Cro    NInpO102Cro                    *campo"normalizzato"
046000000000     c                   SETOFF                                           98
046100000000     c                   Z-ADD     1             i
046200000000     c     NInpO102Cro   LOOKUP    SCro(i)                                98
046300000000if  4c                   IF        NOT *in98                                    *NON trovato
046400000000     c                   MOVEL     '1'           werr                           *errore Output
046500000000     c                   LEAVE
046600000000     c*
046700000000     c* legge e controlla il cliente ritiro in anagrafica
046800000000x   4c                   ELSE                                                   *trovato
046900000000     c                   EXSR      LetAcrCro
047000000000     c*
047100000000     c* imposta in Output i dati presi dall' anagrafica
047200000000if  5c                   IF        werr = '0'                                   *esiste
047300000000     c                   EXSR      ImpDsOutA
047400000000e   5c                   ENDIF
047500000000e   4c                   ENDIF
047600000000e   3c                   ENDIF
047700000000e   2c                   ENDIF
047800000000     c*
047900000000     c* occorre immettere o il codice cliente ritiro o i dati del ritiro
048000000000if  2c                   IF        werr = '0'                                   *ancora valido
048100000000if  3c                   IF        InpO102Cro <> *blanks AND                    *immesso codice
048200050207     c                             InpO102Cro <>  *zeros
048300000000x   3c                   ELSE                                                   *NON immesso codice
048400000000if  4c                   IF        Inpo102Rsr = *blanks                         *SI altri dati
048500000000     c                   MOVEL     '1'           werr                           *SI errore
048600000000     c                   LEAVE
048700000000e   4c                   ENDIF
048800000000e   3c                   ENDIF
048900000000e   2c                   ENDIF
049000000000     C*
049100000000e   1C                   ENDDO
049200000000     c*
049300000000     c* se errore, memorizza il codice errore nella schiera errori
049400000000if  1c                   IF        werr = '1'
049500000000if  2c                   IF        $e < 10                                      *non + di 10 errori
049600000000     c                   ADD       1             $e
049700000000     c                   MOVEL     '3'           Serr($e)                       *aggiunge errore
049800060601     c*                  MOVEL     msg(3)        Smsg($e)                       *messaggio
049900060609     c                   EVAL      Smsg($e) = rtvMsgLang('TIS0068'
050000060609     C                             :inpO102Lang)
050100000000     c                   MOVEL     '1'           $err                           *controllore errore
050200000000e   2c                   ENDIF
050300000000e   1c                   ENDIF
050400000000     c*
050500000000     c                   ENDSR
050600000000     c*--------------------------------------------------------------------------------------------*
050700000000     c* legge e controlla il cliente ritiro in anagrafica
050800000000     c*--------------------------------------------------------------------------------------------*
050900000000     c     LetAcrCro     BEGSR
051000000000     c*
051100000000     c                   MOVEL     NInpO102Cro   kcrcro
051200000000     c     keyacr        CHAIN     fnacr01l                           98
051300000000if  1c                   IF        NOT *in98        AND                         *esiste
051400000000     c                             acratb = *blanks AND                         *NO annullati
051500000000     c                             acrtcr <> 'M'                                *NO infrequenti
051600000000x   1c                   ELSE                                                   *NON esiste
051700000000     c                   MOVEL     '1'           werr                           *errore
051800000000e   1c                   ENDIF
051900000000     c*
052000000000     c                   ENDSR
052100000000     c*--------------------------------------------------------------------------------------------*
052200000000     c* imposta i campi della DS di Output - parte A
052300000000     c*--------------------------------------------------------------------------------------------*
052400000000     c     ImpDsOutA     BEGSR
052500000000     c*
052600000000     c                   MOVEL     acrcro        OutO102Cro
052700000000     c                   EVAL      OutO102Rsr = acrRsr
052800000000     c                   EVAL      OutO102Inr = acrinr
052900000000     c                   EVAL      OutO102Car = acrCar
053000000000     c                   EVAL      OutO102Lor = acrLor
053100000000     c                   EVAL      OutO102Prr = acrPrr
053200000000     c                   EVAL      OutO102Nar = acrNar
053300000000     c                   EVAL      OutO102Per = acrPer
053400000000     c                   EVAL      OutO102Tel = acrTel
053500000000     c                   EVAL      OutO102Ntm = acrNtm
053600000000     c                   EVAL      OutO102No1 = acrNo1
053700000000     c                   EVAL      OutO102Znr = acrZnr
053800000000     c                   EVAL      OutO102Fcl = acrFcl
053900000000     c                   EVAL      OutO102Fpk = acrFpk
054000000000     c                   EVAL      OutO102Fmc = acrFmc
054100000000     c                   EVAL      OutO102Fbn = acrFbn
054200000000     c                   EVAL      OutO102Fbl = acrFbl
054300000000     c                   EVAL      OutO102Fmt = acrFmt
054400000000     c                   EVAL      OutO102Fat = acrFat
054500000000     c*
054600000000     c                   ENDSR
054700000000     c*--------------------------------------------------------------------------------------------*
054800000000     c* controlla il nominativo ritiro
054900000000     c*--------------------------------------------------------------------------------------------*
055000000000     c     ChkInpRsr     BEGSR
055100000000     c*
055200000000     c* reimposta le variabili di lavoro
055300000000     c                   MOVEL     '0'           werr                           *NO errore
055400000000     c                   CLEAR                   NInpo102Rsr                    *campo"normalizzato"
055500000000     c*
055600000000     c* campo obbligatorio se non inerito il codice cliente ritiro
055700000000do  1C                   DO
055800050207if  2c                   IF        (InpO102Cro = *blanks OR
055900050207     C                             InpO102Cro = *zeros)  AND
056000000000     c                             Inpo102Rsr = *blanks
056100000000     c                   MOVEL     '1'           werr                           *SI errore
056200000000     c                   LEAVE
056300000000e   2c                   ENDIF
056400000000     c*
056500000000e   1c                   ENDDO
056600000000     c*
056700000000     c* trasforma il campo immesso da minuscolo a maiuscolo
056800000000     c     minu:maiu     XLATE     Inpo102Rsr    NInpo102Rsr                    *Minus -> Maiuscolo
056900000000     C*
057000000000     c*
057100000000     c* se errore, memorizza il codice errore nella schiera errori
057200000000if  1c                   IF        werr = '1'
057300000000if  2c                   IF        $e < 10                                      *non + di 10 errori
057400000000     c                   ADD       1             $e
057500000000     c                   MOVEL     '4'           Serr($e)                       *aggiunge errore
057600060601     c*                  MOVEL     msg(4)        Smsg($e)                       *messaggio
057700060609     c                   EVAL      Smsg($e) = rtvMsgLang('TIS0481'
057800060609     C                             :inpO102Lang)
057900000000     c                   MOVEL     '1'           $err                           *controllore errore
058000000000e   2c                   ENDIF
058100000000e   1c                   ENDIF
058200000000     c*
058300000000     c                   ENDSR
058400000000     c*--------------------------------------------------------------------------------------------*
058500000000     c* controlla l' indirizzo ritiro
058600000000     c*--------------------------------------------------------------------------------------------*
058700000000     c     ChkInpInr     BEGSR
058800000000     c*
058900000000     c* reimposta le variabili di lavoro
059000000000     c                   MOVEL     '0'           werr                           *NO errore
059100000000     c                   CLEAR                   NInpo102Inr                    *campo"normalizzato"
059200000000     c*
059300000000     c* campo obbligatorio se non inerito il codice cliente ritiro
059400000000do  1C                   DO
059500050207if  2c                   IF        (InpO102Cro = *blanks OR
059600050207     C                             InpO102Cro = *zeros)  AND
059700000000     c                             Inpo102inr = *blanks
059800000000     c                   MOVEL     '1'           werr                           *SI errore
059900000000     c                   LEAVE
060000000000e   2c                   ENDIF
060100000000     C*
060200000000e   1c                   ENDDO
060300000000     c*
060400000000     c* trasforma il campo immesso da minuscolo a maiuscolo
060500000000     c     minu:maiu     XLATE     Inpo102Inr    NInpo102Inr                    *Minus -> Maiuscolo
060600000000     c*
060700000000     c* se errore, memorizza il codice errore nella schiera errori
060800000000if  1c                   IF        werr = '1'
060900000000if  2c                   IF        $e < 10                                      *non + di 10 errori
061000000000     c                   ADD       1             $e
061100000000     c                   MOVEL     '5'           Serr($e)                       *aggiunge errore
061200060601     c*                  MOVEL     msg(5)        Smsg($e)                       *messaggio
061300060609     c                   EVAL      Smsg($e) = rtvMsgLang('TIS0282'
061400060609     C                             :inpO102Lang)
061500000000     c                   MOVEL     '1'           $err                           *controllore errore
061600000000e   2c                   ENDIF
061700000000e   1c                   ENDIF
061800000000     c*
061900000000     c                   ENDSR
062000000000     c*--------------------------------------------------------------------------------------------*
062100000000     c* controlla il CAP ritiro
062200000000     c*--------------------------------------------------------------------------------------------*
062300000000     c     ChkInpCar     BEGSR
062400000000     c*
062500000000     c* reimposta le variabili di lavoro
062600000000     c                   MOVEL     '0'           werr                           *NO errore
062700000000     c                   CLEAR                   NInpo102Car                    *campo"normalizzato"
062800000000     c*
062900000000     c* campo obbligatorio se non inerito il codice cliente ritiro
063000000000do  1C                   DO
063100050207if  2c                   IF        (InpO102Cro = *blanks OR
063200050207     C                             InpO102Cro = *zeros)  AND
063300000000     c                             Inpo102Car = *blanks
063400000000     c                   MOVEL     '1'           werr                           *SI errore
063500000000     c                   LEAVE
063600000000e   2c                   ENDIF
063700000000     C*
063800000000e   1c                   ENDDO
063900000000     c*
064000000000     c* trasforma il campo immesso da minuscolo a maiuscolo
064100000000     c     minu:maiu     XLATE     Inpo102Car    NInpo102Car                    *Minus -> Maiuscolo
064200000000     c*
064300000000     c* se errore, memorizza il codice errore nella schiera errori
064400000000if  1c                   IF        werr = '1'
064500000000if  2c                   IF        $e < 10                                      *non + di 10 errori
064600000000     c                   ADD       1             $e
064700000000     c                   MOVEL     '6'           Serr($e)                       *aggiunge errore
064800060601     c*                  MOVEL     msg(6)        Smsg($e)                       *messaggio
064900060609     c                   EVAL      Smsg($e) = rtvMsgLang('TIS0052'
065000060609     C                             :inpO102Lang)
065100000000     c                   MOVEL     '1'           $err                           *controllore errore
065200000000e   2c                   ENDIF
065300000000e   1c                   ENDIF
065400000000     c*
065500000000     c                   ENDSR
065600000000     c*--------------------------------------------------------------------------------------------*
065700000000     c* controlla la località ritiro
065800000000     c*--------------------------------------------------------------------------------------------*
065900000000     c     ChkInpLor     BEGSR
066000000000     c*
066100000000     c* reimposta le variabili di lavoro
066200000000     c                   MOVEL     '0'           werr                           *NO errore
066300000000     c                   CLEAR                   NInpo102Lor                    *campo"normalizzato"
066400000000     c*
066500000000     c* campo obbligatorio se non inerito il codice cliente ritiro
066600000000do  1C                   DO
066700050207if  2c                   IF        (InpO102Cro = *blanks OR
066800050207     C                             InpO102Cro = *zeros)  AND
066900000000     c                             Inpo102Lor = *blanks
067000000000     c                   MOVEL     '1'           werr                           *SI errore
067100000000     c                   LEAVE
067200000000e   2c                   ENDIF
067300000000     C*
067400000000e   1c                   ENDDO
067500000000     c*
067600000000     c* trasforma il campo immesso da minuscolo a maiuscolo
067700000000     c     minu:maiu     XLATE     Inpo102Lor    NInpo102Lor                    *Minus -> Maiuscolo
067800000000     c*
067900000000     c* se errore, memorizza il codice errore nella schiera errori
068000000000if  1c                   IF        werr = '1'
068100000000if  2c                   IF        $e < 10                                      *non + di 10 errori
068200000000     c                   ADD       1             $e
068300000000     c                   MOVEL     '7'           Serr($e)                       *aggiunge errore
068400060601     c*                  MOVEL     msg(7)        Smsg($e)                       *messaggio
068500060609     c                   EVAL      Smsg($e) = rtvMsgLang('TIS0346'
068600060609     C                             :inpO102Lang)
068700000000     c                   MOVEL     '1'           $err                           *controllore errore
068800000000e   2c                   ENDIF
068900000000e   1c                   ENDIF
069000000000     c*
069100000000     c                   ENDSR
069200000000     c*--------------------------------------------------------------------------------------------*
069300000000     c* controlla la provincia ritiro
069400000000     c*--------------------------------------------------------------------------------------------*
069500000000     c     ChkInpPrr     BEGSR
069600000000     c*
069700000000     c* reimposta le variabili di lavoro
069800000000     c                   MOVEL     '0'           werr                           *NO errore
069900000000     c                   CLEAR                   NInpo102Prr                    *campo"normalizzato"
070000000000     c*
070100000000     c* controlla se il dato deve essere immesso oppure no
070200000000do  1c                   DO
070300000000     c*
070400000000     c* trasforma il campo immesso da minuscolo a maiuscolo
070500000000     c     minu:maiu     XLATE     Inpo102Prr    NInpo102Prr                    *Minus -> Maiuscolo
070600000000     C*
070700000000     c* controlla esistenza codice in anagrafica
070800000000if  2c                   IF        NInpo102Prr <> *blanks                       *se inserito
070900000000     c                   SETOFF                                           98
071000000000     c                   Z-ADD     1             i
071100000000     c     NInpo102Prr   LOOKUP    SPrv(i)                                98
071200000000if  3c                   IF        NOT *in98                                    *NON trovata
071300000000     c                   MOVEL     '1'           werr                           *SI errore
071400000000     c                   LEAVE
071500000000e   3c                   ENDIF
071600000000e   2c                   ENDIF
071700000000     c*
071800000000e   1C                   ENDDO
071900000000     c*
072000000000     c* se errore, memorizza il codice errore nella schiera errori
072100000000if  1c                   IF        werr = '1'
072200000000if  2c                   IF        $e < 10                                      *non + di 10 errori
072300000000     c                   ADD       1             $e
072400000000     c                   MOVEL     '8'           Serr($e)                       *aggiunge errore
072500060601     c*                  MOVEL     msg(8)        Smsg($e)                       *messaggio
072600060609     c                   EVAL      Smsg($e) = rtvMsgLang('TIS0464'
072700060609     C                             :inpO102Lang)
072800000000     c                   MOVEL     '1'           $err                           *controllore errore
072900000000e   2c                   ENDIF
073000000000e   1c                   ENDIF
073100000000     c*
073200000000     c                   ENDSR
073300000000     c*--------------------------------------------------------------------------------------------*
073400000000     c* controlla la nazione ritiro
073500000000     c*--------------------------------------------------------------------------------------------*
073600000000     c     ChkInpNar     BEGSR
073700000000     c*
073800000000     c* reimposta le variabili di lavoro
073900000000     c                   MOVEL     '0'           werr                           *NO errore
074000000000     c                   CLEAR                   NInpo102Nar                    *campo"normalizzato"
074100000000     c*
074200000000     c* controlla se il dato deve essere immesso oppure no
074300000000do  1c                   DO
074400000000     c*
074500000000     c* trasforma il campo immesso da minuscolo a maiuscolo
074600000000     c     minu:maiu     XLATE     Inpo102Nar    NInpo102Nar                    *Minus -> Maiuscolo
074700000000     c*
074800000000     c* controlla esistenza codice in anagrafica
074900000000if  2c                   IF        NInpo102Nar <> *blanks                       *se inserito
075000000000     c                   SETOFF                                           98
075100000000     c                   Z-ADD     1             i
075200000000     c     NInpo102Nar   LOOKUP    SNaz(i)                                98
075300000000if  3c                   IF        NOT *in98                                    *NON trovata
075400000000     c                   MOVEL     '1'           werr                           *SI errore
075500000000     c                   LEAVE
075600000000e   3c                   ENDIF
075700000000e   2c                   ENDIF
075800000000     c*
075900000000e   1C                   ENDDO
076000000000     c*
076100000000     c* se errore, memorizza il codice errore nella schiera errori
076200000000if  1c                   IF        werr = '1'
076300000000if  2c                   IF        $e < 10                                      *non + di 10 errori
076400000000     c                   ADD       1             $e
076500000000     c                   MOVEL     '9'           Serr($e)                       *aggiunge errore
076600060601     c*                  MOVEL     msg(9)        Smsg($e)                       *messaggio
076700060609     c                   EVAL      Smsg($e) = rtvMsgLang('TIS0385'
076800060609     C                             :inpO102Lang)
076900000000     c                   MOVEL     '1'           $err                           *controllore errore
077000000000e   2c                   ENDIF
077100000000e   1c                   ENDIF
077200000000     c*
077300000000     c                   ENDSR
077400000000     c*--------------------------------------------------------------------------------------------*
077500000000     c* controlla l' indirizzo + il CAP + la località + la provincia + la nazione ritiro
077600000000     c*--------------------------------------------------------------------------------------------*
077700000000     c     ChkInpICKPNr  BEGSR
077800000000     c*
077900000000     c* reimposta le variabili di lavoro
078000000000     c                   MOVEL     '0'           werr                           *NO errore
078100000000     c                   Z-ADD     *zeros        deplna                         *deposito LNA
078200000000     c*
078300000000     c* controlla se il dato deve essere controllato oppure no
078400000000do  1c                   DO
078500000000if  2c                   IF        InpO102Cro <> *blanks AND
078600050207     c                             InpO102Cro <>  *zeros
078700000000x   2c                   ELSE
078800000000     c*
078900000000     c* controlla esattezza dati ritiro
079000000000     C                   CLEAR                   tisi95ds
079100000000     C                   MOVEL     ' '           I95tla
079200000000     C                   MOVEL     '7'           I95tcn
079300000000     C                   MOVEL     NInpO102Nar   I95nar
079400000000     C                   MOVEL     NInpO102Car   I95cap
079500000000     C                   MOVEL     NInpO102Lor   I95loc
079600000000     C                   MOVEL     NInpO102Prr   I95prv
079700000000     C                   MOVEL     NInpO102Inr   I95ind
079800000000     C                   Z-ADD     datcor        I95dat
079900120829     C                   IF        i95nar = IRLANDA
080000120829     C                   EVAL      i95cap = EIRE
080100120829     C                   ENDIF
080200000000     C                   CALL      'TISI95R'
080300000000     C                   PARM                    tisi95ds
080400000000     c*
080500000000     c* se ritorna errore o dato non affidabile imposta errore
080600000000if  3c                   IF        NInpo102Nar = *blanks  AND                   *ITALIA
080700000000     c                             O95Lia <> '3'          OR                    *affidabilità 3
080800000000     c                             NInpo102Nar <> *blanks AND                   *ESTERO
080900000000     c                             O95Lia <> '2'                                *affidabilità 2
081000000000     c                   MOVEL     '1'           werr                           *SI errore
081100000000     c                   LEAVE
081200000000x   3c                   ELSE
081300000000     c                   Z-ADD     O95Lna        depLna                         *linea arrivo ritiro
081400000000e   3c                   ENDIF
081500000000e   2c                   ENDIF
081600000000     c*
081700000000e   1C                   ENDDO
081800000000     c*
081900000000     c* se errore, memorizza il codice errore nella schiera errori
082000000000if  1c                   IF        werr = '1'
082100000000if  2c                   IF        $e < 10                                      *non + di 10 errori
082200000000     c                   ADD       1             $e
082300000000     c                   MOVEL     'A'           Serr($e)                       *aggiunge errore
082400060601     c*                  MOVEL     msg(10)       Smsg($e)                       *messaggio
082500060609     c                   EVAL      Smsg($e) = rtvMsgLang('TIS0054'
082600060609     C                             :inpO102Lang)
082700000000     c                   MOVEL     '1'           $err                           *controllore errore
082800000000e   2c                   ENDIF
082900000000e   1c                   ENDIF
083000000000     C*
083100000000     C                   ENDSR
083200000000     c*--------------------------------------------------------------------------------------------*
083300000000     c* controlla il pagamento
083400000000     c*--------------------------------------------------------------------------------------------*
083500000000     c     ChkInpPag     BEGSR
083600000000     c*
083700000000     c* reimposta le variabili di lavoro
083800000000     c                   MOVEL     '0'           werr                           *NO errore
083900000000     c                   CLEAR                   NInpo102Pag                    *campo"normalizzato"
084000000000     c*
084100000000     c* controlla validità scelta
084200000000do  1c                   DO
084300050701     c*
084400000000if  2c                   IF        Inpo102Pag = 'M' OR
084500000000     c                             Inpo102Pag = 'D' OR
084600000000     c                             Inpo102Pag = 'O' AND                         *ordinante SOLO SE
084700000000     c                             InpO102Ksu > *zeros                          *cliente PASSWORD
084800000000x   2c                   ELSE
084900000000     c                   MOVEL     '1'           werr                           *SI errore
085000000000     c                   LEAVE
085100000000e   2c                   ENDIF
085200050705     **?Quando l'utente iscritto chiede il ritiro in altra località
085300050701     **?il pagamento non può essere a carico del mittente.
085400050705     C                   IF        inpo102cro = *BLANK AND inpo102pag = 'M' AND
085500050705     C                             inpo102rqscid > 0
085600050701     C                   EVAL      werr = *ON
085700050701     c                   LEAVE
085800050701     C                   ENDIF
085900000000     c*
086000000000e   1C                   ENDDO
086100000000     c*
086200000000     c* se errore, memorizza il codice errore nella schiera errori
086300000000if  1c                   IF        werr = '1'
086400000000if  2c                   IF        $e < 10                                      *non + di 10 errori
086500000000     c                   ADD       1             $e
086600000000     c                   MOVEL     'B'           Serr($e)                       *aggiunge errore
086700060601     c*                  MOVEL     msg(11)       Smsg($e)                       *messaggio
086800060609     c                   EVAL      Smsg($e) = rtvMsgLang('TIS0140'
086900060609     C                             :inpO102Lang)
087000000000     c                   MOVEL     '1'           $err                           *controllore errore
087100000000e   2c                   ENDIF
087200000000e   1c                   ENDIF
087300090211     ** Ritiro all'estero, non può pagare il mittente.
087400090211     c                   IF        wErr = '0' AND Inpo102Pag = 'M' AND (
087500090211     C                             (inpo102cro <> *BLANK
087600090212     c                              AND acrNar <> *BLANK
087700090212     c                              AND acrNar <> 'SM')
087800090211     c                             OR
087900090211     C                             (inpo102cro = *BLANK
088000090212     c                              AND inpo102nar <> *BLANK
088100090212     c                              AND inpo102nar <> 'SM')
088200090212     c                             )
088300090211     c                   IF        $e < 10                                      *non + di 10 errori
088400090211     c                   ADD       1             $e
088500090211     c                   MOVEL     'B'           Serr($e)                       *aggiunge errore
088600090211     c                   EVAL      Smsg($e) = rtvMsgLang('TIS0695'
088700090211     C                             :inpO102Lang)
088800090211     c                   MOVEL     '1'           $err                           *controllore errore
088900090211     ** Pulisco i dati di output relativi al mittente, altrimenti imposterei
089000090211     ** il form relativo al mittente non codificato creando un conflitto di dati.
089100090211     C                   IF        inpo102cro <> *BLANK
089200090211     c                   CLEAR                   OutO102Rsr
089300090211     c                   CLEAR                   OutO102Inr
089400090211     c                   CLEAR                   OutO102Car
089500090211     c                   CLEAR                   OutO102Lor
089600090211     c                   CLEAR                   OutO102Prr
089700090211     c                   CLEAR                   OutO102Nar
089800090211     C                   ENDIF
089900090211     c                   ENDIF
090000090211     c                   ENDIF
090100090211     c*
090200000000     c                   ENDSR
090300000000     c*--------------------------------------------------------------------------------------------*
090400000000     c* edita data da aaaammgg in gg/mm/aaaa
090500000000     c*--------------------------------------------------------------------------------------------*
090600000000     c     edtdat        BEGSR
090700000000     c*
090800000000     c                   MOVEL     *blanks       wdate
090900000000     c*
091000000000     c                   MOVEL     wdat          dsdat
091100000000     c                   MOVEL     dsgio         dsgioe
091200000000     c                   MOVEL     '/'           dsvd1
091300000000     c                   MOVEL     dsmes         dsmese
091400000000     c                   MOVEL     '/'           dsvd2
091500000000     c                   MOVEL     dsann         dsanne
091600000000     c                   MOVEL     dsdate        wdate
091700000000     c*
091800000000     c                   ENDSR
091900000000     c*--------------------------------------------------------------------------------------------*
092000000000     c* edita ora da hhmm in hh:mm
092100000000     c*--------------------------------------------------------------------------------------------*
092200000000     c     edtora        BEGSR
092300000000     c*
092400000000     c                   MOVEL     *blanks       worae
092500000000     c*
092600000000     c                   MOVEL     wora          dsora
092700000000     c                   MOVEL     dshh          dshhe
092800000000     c                   MOVEL     ':'           dsvo1
092900000000     c                   MOVEL     dsmm          dsmme
093000000000     c                   MOVEL     dsorae        worae
093100000000     c*
093200000000     c                   ENDSR
093300000000     c*--------------------------------------------------------------------------------------------*
093400000000     c* imposta i campi della DS di Output - parte B
093500000000     c*--------------------------------------------------------------------------------------------*
093600000000     c     ImpDsOutB     BEGSR
093700000000     c*
093800000000     c* se immesso codice cliente ritiro alcuni dati di Output sono già stati
093900000000     c* impostati nella routine ImpDsOutA
094000000000if  1c                   IF        OutO102Cro <> *blanks AND
094100000000     c                             OutO102Cro >  *zeros
094200000000x   1c                   ELSE
094300000000     c*
094400000000     c* se immessi dati ritiro, i dati di Output vengono impostati ora
094500000000     c                   EVAL      OutO102Cro = *zeros
094600000000     c                   EVAL      OutO102Rsr = NInpO102Rsr
094700000000     c                   EVAL      OutO102Inr = NInpO102Inr
094800000000     c                   EVAL      OutO102Car = NInpO102Car
094900000000     c                   EVAL      OutO102Lor = NInpO102Lor
095000000000     c                   EVAL      OutO102Prr = NInpO102Prr
095100000000     c                   EVAL      OutO102Nar = NInpO102Nar
095200000000e   1c                   ENDIF
095300000000     c*--
095400000000     c* calcola alcuni dati
095500000000     c*---
095600000000     c*******************
095700021108     c* Prepagato --> Cliente senza PASSWORD: se pagamento <> destinatario
095800021108     c*******************
095900000000if  1c                   IF        InpO102Ksu > *zeros                          *cliente con PW
096000011108x   1C                   ELSE                                                   *cliente senza PW
096100000000if  2c                   IF        InpO102Pag <> 'D'                            *NO pag.destinatario
096200000000     c                   EVAL      OutO102PPG = 'S'                             *SI prepagato
096300000000e   2c                   ENDIF
096400000000e   1c                   ENDIF
096500000000     c*******************
096600000000     c* PO ritiro --> se NON immesso cliente ritiro --> PO calcolato da CAP
096700021108     c*               se     immesso cliente ritiro --> PO di gestione del cliente ritiro
096800000000     c*******************
096900000000if  1c                   IF        OutO102Cro <> *blanks AND                    *SI cliente ritiro
097000000000     c                             OutO102Cro >  *zeros
097100021108     c*---               MOVEL     NinpO102Cro   OutO102Por
097200021108     c                   MOVEL     acrpoa        OutO102Por
097300000000x   1c                   ELSE
097400000000     c                   MOVEL     deplna        OutO102Por
097500000000e   1c                   ENDIF
097600000000     c*******************
097700021108     c* PO emittente > Cliente con   PWD: se NON immesso c.ritiro --> PO cliente PASSWORD
097800021108     c*                                   se     immesso c.ritiro --> PO di gestione del c.ritiro
097900021108     c*                Cliente senza PWD:                         --> PO calcolato da CAP
098000000000     c*******************
098100000000if  1c                   IF        InpO102Ksu > *zeros                          *SI cliente PW
098200000000if  2c                   IF        OutO102Cro <> *blanks AND                    *SI cliente ritiro
098300000000     c                             OutO102Cro >  *zeros
098400021108     c*---               MOVEL     NinpO102Cro   OutO102Poe
098500021108     c                   MOVEL     acrpoa        OutO102Poe
098600000000x   2c                   ELSE                                                   *NO cliente ritiro
098700000000     c                   EVAL      OutO102Poe = %SUBST(InpO102Ksu:2:3)
098800000000e   2c                   ENDIF
098900000000X   1C                   ELSE                                                   *NO cliente PW
099000000000     c                   MOVEL     deplna        OutO102Poe
099100000000e   1c                   ENDIF
099200000000     c*******************
099300000000     c* Ritiro alle'estero (S/N)
099400000000     c*******************
099500000000     c                   MOVEL     OutO102Por    n3
099600000000     c                   SETOFF                                           98
099700000000     c                   Z-ADD     1             i
099800000000     c     n3            LOOKUP    sorg(i)                                98
099900000000if  1c                   IF        *in98 AND                                    *trovata
100000000000     c                             SOFl1(i) = 'E'                               *filiale estera
100100000000     c                   EVAL      OutO102Fre = 'S'                             *SI ritiro estero
100200000000x   1c                   ELSE
100300000000     c                   EVAL      OutO102Fre = 'N'                             *NO ritiro estero
100400000000e   1c                   ENDIF
100500000000     c*******************
100600000000     c* Impostazione scelta servizio estero -->
100700000000     c*   Cliente con PASSWORD: se appartiene a una filiale DPD imposta il servizio DPD, se filiale
100800000000     c*   EuroExpress imposta il servizio EuroExpress, imposta la NON scelta
100900000000     c*******************
101000000000if  1c                   IF        InpO102Ksu > *zeros                          *cliente con PW
101100000000     c                   EVAL      a3 = %SUBST(InpO102Ksu:2:3)                  *filiale cliente PW
101200000000     c                   MOVEL     a3            n3
101300000000     c                   SETOFF                                           98
101400000000     c                   Z-ADD     1             i
101500000000     c     n3            LOOKUP    sorg(i)                                98
101600000000if  2c                   IF        *in98                                        *trovata
101700000000if  3c                   IF        SOFl1(i) = 'E'                               *filiale estera
101800000000     c                   EVAL      OutO102Ose = 'E'                             *servizio EExpress
101900000000e   3c                   ENDIF
102000020702if  3c                   IF        SODpd(i) = 'DPD'                             *filiale DpD
102100000000     c                   EVAL      OutO102Ose = 'D'                             *servizio DpD
102200000000e   3c                   ENDIF
102300000000e   2c                   ENDIF
102400000000e   1c                   ENDIF
102500000000     c*
102600000000     c                   ENDSR
102700000000     c*--------------------------------------------------------------------------------------------*
102800000000     c* caricamento tabelle occorrenti
102900000000     c*--------------------------------------------------------------------------------------------*
103000000000     c     cartab        BEGSR
103100000000     c*---
103200000000     c* organigramma
103300000000     c*---
103400000000     c                   CLEAR                   i                              *indice
103500000000     C                   CLEAR                   sorg
103600000000     C                   CLEAR                   sodes
103700000000     C                   CLEAR                   sofl1
103800000000     C                   CLEAR                   sopt
103900000000     C                   CLEAR                   sodpd
104000000000     c     *loval        SETLL     azorg01l
104100000000     c                   READ      azorg01l                               99
104200000000do  1c                   DOW       NOT *in99
104300000000     c                   ADD       1             i
104400000000     c                   Z-ADD     orgfil        sorg(i)
104500000000     c                   MOVEL     orgdes        sodes(i)
104600020702     c                   MOVEL     orgde3        og143
104700020702     C*
104800020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
104900020702     C                   CLEAR                   dntw
105000020702     C*
105100020702     C                   MOVEL(P)  'NTW'         kbecod
105200020702     C                   MOVEL(P)  §ogntw        kbeke1
105300020702     C     keytbe        CHAIN     tntbe01l
105400020702     C                   IF        %found(tntbe01l)
105500020702     C                   MOVEL     tbeuni        dntw
105600020702     C                   ENDIF
105700020702     C*
105800020702     C                   MOVEL     §ntwfie       sofl1(i)
105900020702     C                   IF        §ogntw = 'DPD'
106000020702     C                   MOVEL     'E'           sofl1(i)
106100020702     C                   ENDIF
106200020702     C*
106300020702     C                   MOVEL     §ogntw        sopt(i)
106400020702     C                   MOVEL     §ogntw        sodpd(i)
106500020702     C                   READ      azorg01l                               99
106600020702e   1C                   ENDDO
106700000000     c*---
106800000000     c* provincie
106900000000     c*---
107000000000     c                   CLEAR                   i                              *indice
107100000000     C                   CLEAR                   dspr
107200000000     C                   CLEAR                   SPrv
107300000000     C                   CLEAR                   SPDes
107400020906     C                   Z-ADD     1             kblkut
107500000000     C                   MOVEL     'PR'          kblcod
107600000000     c     keytab        SETLL     tabel00f
107700000000     c     keytab        READE     tabel00f                               99
107800000000do  1c                   DOW       NOT *in99
107900000000if  2c                   IF        tblflg = *blanks
108000000000     c                   MOVEL     tbluni        dspr
108100000000     c                   ADD       1             i
108200000000     c                   MOVEL     tblkey        SPrv(i)
108300000000     c                   MOVEL     §Prdes        SPDes(i)
108400000000e   2c                   ENDIF
108500000000     c     keytab        READE     tabel00f                               99
108600000000e   1c                   ENDDO
108700000000     c*---
108800000000     c* nazioni
108900000000     c*---
109000000000     c                   CLEAR                   i                              *indice
109100000000     C                   CLEAR                   ds15
109200000000     C                   CLEAR                   SNaz
109300000000     C                   CLEAR                   SNDes
109400020906     C                   Z-ADD     1             kblkut
109500000000     C                   MOVEL     '15'          kblcod
109600000000     c     keytab        SETLL     tabel00f
109700000000     c     keytab        READE     tabel00f                               99
109800000000do  1c                   DOW       NOT *in99
109900000000if  2c                   IF        tblflg = *blanks
110000000000     c                   MOVEL     tbluni        ds15
110100000000     c                   ADD       1             i
110200000000     c                   MOVEL     tblkey        SNaz(i)
110300000000     c                   MOVEL     §15des        SNDes(i)
110400000000e   2c                   ENDIF
110500000000     c     keytab        READE     tabel00f                               99
110600000000e   1c                   ENDDO
110700000000     c*
110800000000     c                   ENDSR
110900000000     c*--------------------------------------------------------------------------------------------*
111000000000     c* operazioni iniziali -da fare sempre-
111100000000     c*--------------------------------------------------------------------------------------------*
111200000000     c     rutinz        BEGSR
111300000000     c*
111400000000     c* reperimento data corrente
111500000000     C                   TIME                    n14                            *ora (6)+ data (8)
111600000000     C                   MOVEL     n14           oracor                         *ora  (6) in h:m:s
111700000000     C                   MOVE      n14           n8                             *data (8) in g/m/a
111800000000     C                   Z-ADD     n8            g08dat
111900000000     C                   Z-ADD     *zeros        g08inv
112000000000     C                   MOVEL     '0'           g08err
112100000000     c                   CALL      'XSRDA8'
112200000000     c                   PARM                    wlbda8
112300000000     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
112400000000     c*
112500000000     c* azzera la DS di Output e le variabili di lavro
112600000000     c                   CLEAR                   tio102ODs                      *Ds Output
112700000000     c                   MOVEL     *ALL'0'       Outo102Err                     *NO errore
112800000000     c                   MOVEL     *all'0'       SErr                           *schiera errori
112900000000     c                   MOVEL     *blanks       SMsg                           *schiera msg errori
113000000000     c                   Z-ADD     *zeros        $e                             *indice  errori
113100000000     c                   MOVEL     '0'           $err                           *controllore errore
113200000000     c*
113300000000     c* azzera le variabili "normalizzate"
113400000000     c                   CLEAR                   NInpo102Cfo
113500000000     c                   CLEAR                   NInpo102Pio
113600000000     c                   CLEAR                   NInpo102Cro
113700000000     c                   CLEAR                   NInpo102Rsr
113800000000     c                   CLEAR                   NInpo102Inr
113900000000     c                   CLEAR                   NInpo102Car
114000000000     c                   CLEAR                   NInpo102Lor
114100000000     c                   CLEAR                   NInpo102Prr
114200000000     c                   CLEAR                   NInpo102Nar
114300000000     c                   CLEAR                   NInpo102Pag
114400000000     c*
114500000000     c* sposta direttamente i campi dell'input in quelli di output
114600000000     c                   MOVEL     InpO102Cro    Outo102Cro
114700000000     c                   MOVEL     InpO102Rsr    Outo102Rsr
114800000000     c                   MOVEL     InpO102Inr    Outo102Inr
114900000000     c                   MOVEL     InpO102Car    Outo102Car
115000000000     c                   MOVEL     InpO102Lor    Outo102Lor
115100000000     c                   MOVEL     InpO102Nar    Outo102Nar
115200000000     c                   MOVEL     InpO102Prr    Outo102Prr
115300000000     c                   MOVEL     InpO102Pag    Outo102Pag
115400000000     c*
115500000000     c                   ENDSR
115600000000     c*--------------------------------------------------------------------------------------------*
115700000000     C* Memorizza in schiere i clienti ritiro dell'utente STRATEGI
115800000000     c*--------------------------------------------------------------------------------------------*
115900000000     C     CarAcr        BEGSR
116000000000     C*
116100000000     C* azzera le variabili di lavoro
116200000000     c                   CLEAR                   i                              *indici
116300000000     c                   CLEAR                   ii
116400000000     c                   CLEAR                   SCro                           *codici clienti
116500000000     C*
116600000000     c* carica i clienti figli legati al cliente unificante di entrata
116700000000     c                   EXSR      CarKun
116800000000     C*
116900000000     C* per ciascun cliente figlio memorizzato (o solo il padre se non ci sono figli)
117000000000     c                   Z-ADD     1             i
117100000000     c                   Z-ADD     *zeros        ii
117200000000do  1C                   DOW       sksc7(i) > *zeros AND                        *piena schiera INP
117300000000     C                             i  <= 500         AND                        *fine  schiera INP
117400000000     C                             ii <  $dim                                   *fine  schiera OUT
117500000000     C*
117600000000     C* memorizza tutti i suoi clienti ritiro + progressivo
117700000000     c                   MOVEL     sksc7(i)      wacr
117800000000     c                   MOVE      000           wacr
117900000000     c                   Z-ADD     wacr          kcrcro
118000000000     c     keyacr        SETLL     fnacr01l
118100000000     c                   READ      fnacr01l                               98
118200000000     c*
118300000000     c* memorizza il cliente ritiro letto (senza prg) in una variabile di lavoro
118400000000     c                   MOVEL     acrcro        wcli                           *cliente senza prg
118500000000     c*
118600000000     c* ciclo fino a fine file o rottura cliente ritiro (senza prg)
118700000000do  2C                   DOW       ii <  $dim        AND                        *fine  schiera OUT
118800000000     c                             NOT *in98         AND                        *fine file
118900000000     c                             sksc7(i) = wcli                              *fine prg cliente
119000000000     c* solo record validi
119100000000if  3c                   IF        acratb = *blanks  AND                        *NO annullati
119200000000     c                             acrtcr <> 'M'                                *NO infrequenti
119300000000     c* memorizza i dati
119400000000     c                   EVAL      ii = ii + 1                                  *indice
119500000000     c                   EVAL      SCro(ii) = acrcro                            *codice cliente
119600000000e   3c                   ENDIF
119700000000     c*
119800000000     c* lettura successiva cliente ritiro
119900000000     c                   READ      fnacr01l                               98
120000000000     c*
120100000000     c* memorizza il cliente ritiro letto (senza prg) in una variabile di lavoro
120200000000     c                   MOVEL     acrcro        wcli                           *cliente senza prg
120300000000e   2c                   ENDDO                                                  *rottura cliente
120400000000     C*
120500000000     c                   ADD       1             i                              *lettura cl.figlio
120600000000e   1c                   ENDDO                                                  *fine clienti figli
120700000000     C*
120800000000     C                   ENDSR
120900000000     c*--------------------------------------------------------------------------------------------*
121000000000     c* Carica i clienti figli legati al cliente unificante di entrata
121100000000     c*--------------------------------------------------------------------------------------------*
121200000000     c     CarKun        BEGSR
121300000000     c*
121400000000     c* reimposta variabili di lavoro
121500000000     c                   CLEAR                   sksc7                          clienti da elaborare
121600000000     c*
121700000000     c* reperisce il legame clienti figli -se c'è- sul file delle abilitazioni
121800000000     c                   EVAL      kssksu = InpO102Ksu                          *cliente unificante
121900000000     c                   EVAL      kssisv = 'TT'                                *tipo servizio
122000000000     c     keyvss        SETLL     tivss02l
122100000000     c     keyvss        READE     tivss02l                               99
122200000000do  1c                   DOW       NOT *in99
122300000000if  2c                   IF        vssvat =  *blanks                            *attivo
122400000000if  3c                   IF        datcor >= vssdde AND                         *in decorrenza
122500000000     c                             datcor <= vssdsc
122600000000     c*
122700000000     c* controlla, se per il cliente unificante, ci sono clienti figli da elaborare
122800000000if  4c                   IF        vsstle <> *blanks                            *SI legami
122900000000     c                   CLEAR                   tibs10ds
123000000000     c                   EVAL      d10drf = datcor                              *data riferimento
123100000000     c                   EVAL      d10tle = vsstle                              *legame
123200000000     c                   EVAL      d10paf = 'F'                                 *cerca i figli
123300000000     c                   MOVEL     vssksu        n8
123400000000     c                   Z-ADD     n8            d10cod                         *cliente unificante
123500000000     c                   CALL      'TIBS10R'                                    *cerca figli
123600000000     c                   PARM                    tibs10ds
123700000000if  5c                   IF        d10err <> ' '                                *SI errore: no figli
123800000000     c                   MOVE      InpO102Ksu    sksc7(1)                       *cliente unificante
123900000000x   5c                   ELSE                                                   *NO errore: si figli
124000000000     c                   Z-ADD     sksc11        sksc7                          *cliente unif+figli
124100000000e   5c                   ENDIF
124200000000x   4c                   ELSE                                                   *NO legami
124300000000     c                   MOVE      InpO102Ksu    sksc7(1)                       *cliente unificante
124400000000e   4c                   ENDIF
124500000000     c*
124600000000     c* se trovato cliente abilitato valido, esce dal ciclo di lettura
124700000000     c                   LEAVE                                                  *uscita ciclo
124800000000e   3c                   ENDIF
124900000000e   2c                   ENDIF
125000000000     c     keyvss        READE     tivss02l                               99
125100000000e   1c                   ENDDO                                                  *fine cl.abilitati
125200000000     c*
125300000000     c                   ENDSR
125400000000     c*--------------------------------------------------------------------------------------------*
125500000000     c* Operazioni iniziali -solo la prima volta-
125600000000     c*--------------------------------------------------------------------------------------------*
125700000000     C     *InzSr        BEGSR
125800000000     c*
125900000000     c* ricevimento parametri
126000000000     c     *ENTRY        PLIST
126100000000     c                   PARM                    tiO102IDS
126200000000     c                   PARM                    tiO102ODS
126300000000     c*
126400000000     c* chiavi di lettura
126500000000     c     keytab        KLIST                                                  tabel00f
126600000000     c                   KFLD                    kblkut                         -utente
126700000000     c                   KFLD                    kblcod                         -tabella
126800000000     c     keytbe        KLIST                                                  *tntbe01l
126900000000     c                   KFLD                    kbecod                          -tabella
127000000000     c                   KFLD                    kbeke1                          -chiave uno
127100000000     c     ke2tbe        KLIST                                                  *tntbe02l
127200000000     c                   KFLD                    kbecod                          -tabella
127300000000     c                   KFLD                    kbelin                          -lingua
127400000000     c                   KFLD                    kbesif                          -s.informativo
127500000000     c     keyvss        KLIST                                                  *tivss02l
127600000000     c                   KFLD                    kssksu                          -cl.unificante
127700000000     c                   KFLD                    kssisv                          -tipo servizio
127800000000     c     keyacr        KLIST                                                  *fnacr01l
127900000000     c                   KFLD                    kcrcro                          -cl.ritiro
128000000000     c*
128100000000     c* agiunge le librerie in lista
128200060601     c*                  EVAL      wpgm = 'GAITRAOBJ/TIS777C'
128300060601     c*                  CALL      wpgm
128400000000     c*
128500000000     c* apre i files
128600000000     c                   OPEN      tivss02l
128700000000     c                   OPEN      fnacr01l
128800000000     c                   OPEN      tabel00f
128900000000     c                   OPEN      tntbe01l
129000000000     c                   OPEN      tntbe02l
129100000000     c                   OPEN      azorg01l
129200000000     c*
129300000000     c* caricamento tabelle occorrenti
129400000000     c                   EXSR      cartab
129500000000     c*
129600000000     c                   ENDSR
129700060601
129800000000** -MSG- Messaggi di uscita                                                        num  cod
129900000000/ Codice fiscale errato                                                             1    1
130000000000/ Partita Iva errata                                                                2    2
130100000000/ Codice cliente ritiro errato                                                      3    3
130200000000/ Ragione sociale errata                                                            4    4
130300000000/ Indirizzo errato                                                                  5    5
130400000000/ CAP errato                                                                        6    6
130500000000/ Località errata                                                                   7    7
130600000000/ Provincia errata                                                                  8    8
130700000000/ Nazione errata                                                                    9    9
130800000000/ CAP, località e provincia incongruenti fra loro                                   10   A
130900000000/ Dati pagamento errati                                                             11   B
131000000000/ Immettere il codice fiscale oppure la partita IVA                                 12   C
131100000000                                                                                    13   D
131200000000                                                                                    14   E
131300000000                                                                                    15   F
131400000000                                                                                    16   G
131500000000                                                                                    17   H
131600000000                                                                                    18   I
131700000000                                                                                    19   L
131800000000                                                                                    20   M
