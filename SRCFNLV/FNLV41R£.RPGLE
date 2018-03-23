000100020909     H DECEDIT('0,') DATEDIT(*YMD.)
000200010404      *---------------------------------------------------------------*
000300021018      * RICERCA TESTI/RESTITUISCE DS1H/SCRIVE RECORD TIPO H DI FNTXT00F
000400010404      *---------------------------------------------------------------*
000500021021     fFnTxt02l  if a e           k disk
000600021018     fTabel00f  if   e           k disk
000700021022     fFnlv41d   cf   e             workstn SFILE(LV41S01:NRR)   USROPN
000800021018      *-------------------
000900021018      * DS ESTERNE
001000021018      *-------------------
001100021018     d Ds1H          e ds                  inz
001200021022     d DfntxtH       e ds                  inz
001300021021     d Ds1I          E DS
001400021022     d  Ski                   26     88    DIM(21)
001500021021     d Fnlv41Ds      e ds
001600021018     d Tibs34Ds      e ds
001700021018     d DDatiUte      e ds
001800021018     d AzUteDs       e ds                  extname(AZUTE00F)
001900021022      *
002000021022     D KUT             S                   LIKE(TBLKUT) INZ(1)
002100021022     D COD             S                   LIKE(TBLCOD) INZ('1H')
002200021022     D COD2            S                   LIKE(TBLCOD) INZ('1I')
002300021022     D KEY             S                   LIKE(TBLKEY)
002400021022     D SAVKEY          S                   LIKE(TBLKEY)
002500021022     D $let            S                   LIKE(Txtlet)
002600021022     D $pol            S                   LIKE(Txtpol)
002700021022     D $pos            S                   LIKE(Txtpos)
002800010404      *---------------------------------------------------------------*
002900010404      *                    M  A  I  N                                 *
003000010404      *---------------------------------------------------------------*
003100020905      *
003200020905      * Procedo se richiesta elaborazione, se non ho errori
003300021018    1c                   IF        I41TLA <> 'C'
003400021018      *
003500021018      * Operazioni iniziali
003600021018     c                   EXSR      Iniz
003700021018      *
003800021021      * Gestisco le modalità di richiamo
003900021021     c                   EXSR      Ges_Mod
004000021018      *
004100021021      * Valorizzo la Ds di ritorno con dati per uso testo
004200021021     c                   EXSR      Ges_Ds
004300021021     c                   ENDIF
004400020905      *
004500020905      * Fine programma
004600021021    1c                   IF        I41TLA <> *blanks
004700021022     c                   eval      *inLR = *on
004800021022     c                   ELSE
004900021022     c                   eval      *inRT = *on
005000021021     c                   ENDIF
005100021018      *----------------------------------------------------------
005200021021      * Gestisco le modalità di richiamo
005300021018      *----------------------------------------------------------
005400021021     c     Ges_Mod       BEGSR
005500021018      *
005600021022      * Modalità 1 Ricerca
005700021021   1ac                   IF        I41Mod = '1'
005800021022      *  Apro file video
005900021021     C                   if        not %OPEN(FNLV41D)
006000021021     c                   open      fnlv41d
006100021021     c                   endif
006200021021      *
006300021021     c                   EXSR      GESMOD1
006400021021      *
006500021022      * Memorizzo il Tipo lettera scelto a sfl
006600021021     c                   move      O41Let        $let
006700021022      *
006800021022   1ec                   ELSE
006900021021      *
007000021022      * Modalità 2 o 3
007100021022      * Memorizzo il Tipo lettera passato a pgm
007200021021     c                   move      I41Let        $let
007300021021   1-c                   ENDIF
007400021021      *
007500021022      * Aggancio tabella 1H per recuperare la DS
007600021021     c                   clear                   Ds1H
007700021022     c                   movel(p)  $Let          key
007800021022     c     ktab          chain     tabel
007900021028      *
008000021028    1c                   SELECT
008100021028      *  Tabel non trovato ERRORE
008200021028     c                   WHEN      NOT %found(tabel00f)
008300021028     c                   move      'E'           O41Err
008400021028      *  Tabel non nullo memorizzo DS
008500021028     c                   WHEN      TBLflg = *blanks
008600021022     c                   movel     tbluni        Ds1H
008700021028      *  Tabel nullo e tipo richiamo "4" memorizzo DS e errore A=rcd nullo
008800021028     c                   WHEN      I41mod = '4'
008900021028     c                   movel     tbluni        Ds1H
009000021028     c                   move      'A'           O41Err
009100021028      *  Tabel nullo e tipo richiamo <> "4" errore E=rcd non trovato
009200021028     C                   OTHER
009300021022     c                   move      'E'           O41Err
009400021028    1C                   ENDSL
009500021021      *
009600050831   1ac                   IF        O41Err <> 'E' and §1html = *blanks
009700021021      *
009800021022      * Cerco il tipo testo H con simfel in chiave
009900021022     c                   clear                   DfntxtH
010000021021     c                   movel     Simfel        $Pol
010100021022     c                   movel     'H'           $Pos
010200021022     c     KTxt          chain     FnTxt02l
010300021021      *
010400021022      * Se ho trovato il record imposto DfntxtH
010500021022      * Se non ho trovato e sono in modalità 3 vedo se esiste testo
010600021022      * "master" da cui copiare
010700021022    2c                   SELECT
010800021022     c                   WHEN      %found(FNTXT02L)
010900021022     c                   movel     TxtRig        DfntxtH
011000021022     c                   WHEN      I41Mod = '3'
011100021022     c                   exsr      GESMOD3
011200021022    2c                   ENDSL
011300021022      *
011400021021    1c                   ENDIF
011500021022      *
011600021018     c                   ENDSR
011700021018      *----------------------------------------------------------
011800021018      * Modalità di richiamo 1
011900021018      *----------------------------------------------------------
012000021018     c     GesMod1       BEGSR
012100021021      *
012200021021      * Caricamento subfile
012300021021     c                   EXSR      CarSf
012400021021      *
012500021022      * Emissione subfile
012600021021    1c                   IF        Nrr > 0
012700021021      *
012800021021     c     SOPRA         TAG
012900021021     c                   z-add     1             rec
013000021021     c                   WRITE     LV41Z01
013100021021     c                   EXFMT     LV41C01
013200021021      *
013300021021      * CMD12 - Ritorno
013400021022    2c                   IF        *inkl = *off
013500021021      *
013600021021      * Controllo dati subfile
013700021021     C                   READC     LV41S01                                31
013800021018     C   31              GOTO      SOPRA
013900021021      *
014000021021     c                   clear                   Ds1H
014100021021     c                   clear                   O41Let
014200021021    3c  N31              IF        VidSce = '1'
014300021021      * tipo lettera
014400021021     c                   movel     VidTtx        O41Let
014500021021      *
014600021021   X3c                   ELSE
014700021021     c                   GOTO      SOPRA
014800021021    3c                   ENDIF
014900021021      *
015000021025     c                   else
015100021025     c                   close     fnlv41d
015200021021    2c                   ENDIF
015300021021    1c                   ENDIF
015400021021      *
015500021021     c                   ENDSR
015600021021      *----------------------------------------------------------
015700021021      * Caricamento sfl
015800021021      *----------------------------------------------------------
015900021021     c     Carsf         BEGSR
016000021021     c                   z-add     0             nrr               4 0
016100021021      *
016200021021      * pulizia subfile
016300021021     c                   seton                                        35
016400021021     c                   WRITE     LV41C01
016500021021     c                   setoff                                       35
016600021021      *
016700021021     c                   movel     ' '           VidSce
016800021021      *
016900021021      * Riempio Sfl
017000021021     c     Ktab1         setll     Tabel
017100021021     c     Ktab1         reade     Tabel                                  30
017200021021      *
017300021021    1c     *in30         doweq     *off
017400021021      * escludo gli annullati
017500021022    2c                   IF        TblFlg = *blanks
017600021022     c                   eval      *in15 = *off
017700021021     c                   movel     tbluni        Ds1H
017800021021      *
017900021021      * Salvo la chiave di lettura testi
018000021022     c                   movel     tblkey        savkey
018100021021      *
018200021021      * Controllo se profilo valido per la gestione del testo richiesto
018300021021    3c                   IF        I41Pro <> *blanks
018400021021      *
018500021022     c                   movel(P)  §1HFgt        key
018600021022     c     Ktab2         chain     Tabel
018700021022    4c                   IF        %found(TABEL00F) and TblFlg = *blanks
018800021021     c                   movel     tbluni        Ds1I
018900021021     c     '999'         lookup    Ski                                    36
019000021021     c  n36I41Pro        lookup    Ski                                    36
019100021022     c  n36              eval      *in15 = *on
019200021022   X4c                   ELSE
019300021022     c                   eval      *in15 = *on
019400021021    4c                   ENDIF
019500021021      *
019600021021    3c                   ENDIF
019700021021      *
019800021021      * I41Prv = 'S'  ====>  carico solo i testi da presentare a menu'
019900021025    3c                   IF        *in15 = *off  and
020000021025    3c                             I41Prv = 'S'  and  §1hfl1 <> 'S'
020100021022     c                   eval      *in15 = *on
020200021021    3c                   ENDIF
020300021021      *
020400021021      * I41Prv = 'G'  ====>  sede:    carico solo i testi della sede
020500021021      *                      filiale: carico solo i testi della filiale
020600021025    3c                   IF        *in15 = *off and I41Prv = 'G'
020700021025      *
020800021025    4c                   IF        (Simfel <> *zeros  and  §1hfge = 'S')
020900021025     c                             or (Simfel = *zeros  and  §1hfge = 'F')
021000021022     c                   eval      *in15 = *on
021100021021    4c                   ENDIF
021200021025      *
021300021021    3c                   ENDIF
021400021021      *
021500021021      * 15 OFF - carico il testo lettera
021600021021    3c                   IF        *in15 = *OFF
021700021021     c                   movel     savkey        VidTtx
021800021021      * recupero descrizione
021900021021     c                   movel     savkey        $let
022000021021     c                   movel     Simfel        $Pol
022100021022     c                   movel     'H'           $Pos
022200021022     c     KTxt          chain     FnTxt02l
022300021022    4c                   IF        %found(fntxt02L)
022400021022     c                   movel     TxtRig        DfntxtH
022500021022     c                   movel     §TXdes        VidDes
022600021022    4c                   ELSE
022700021022     c                   movel     §1HDes        VidDes
022800021022    4c                   ENDIF
022900021021      *
023000021021     c                   add       1             nrr
023100021021     c                   WRITE     LV41S01
023200021021    3c                   ENDIF
023300021021      *
023400021021     c                   movel     savkey        key
023500021021     c     ktab          setgt     TABEL
023600021021    2c                   ENDIF
023700021021     c     ktab1         reade     TABEL                                  30
023800021021    1c                   ENDDO
023900021021      *
024000021021     c                   ENDSR
024100021018      *----------------------------------------------------------
024200021018      * Modalità di richiamo 3
024300021018      *----------------------------------------------------------
024400021018     c     GesMod3       BEGSR
024500021018      *
024600021021      * Se sono sede do errore subito, di testo non trovato,
024700021022      * altrimenti provo a cercare con P.O. zero
024800021022    1c                   IF        Simfel = *zeros
024900021021     c                   move      'T'           O41Err
025000021022   x1c                   ELSE
025100021022      *
025200021022     c                   z-add     *zeros        $Pol
025300021022     c     KTxt1         setll     FnTxt02l
025400021022     c     KTxt1         reade     FnTxt02l                               25
025500021018      *
025600021022    2c                   IF        *in25 = *on
025700021021      * errore di testo non trovato
025800021021     c                   move      'T'           O41Err
025900021022   x2c                   ELSE
026000021021      *
026100021021      * li copio tutti come con P.O lettera uguale a simfel
026200021022      * Quando leggo il tipo testo H memorizzo la DS
026300021022    3c     *in25         DOWEQ     *off
026400021022     c                   z-add     Simfel        TxtPol
026500021018     c                   write     FnTxt000
026600021022     c                   IF        TXTPOS = 'H'
026700021022     c                   movel     TxtRig        DfntxtH
026800021022     c                   Endif
026900021022     c     KTxt1         reade     FnTxt02l                               25
027000021022    3c                   ENDDO
027100021022      *
027200021022    2c                   ENDIF
027300021022      *
027400021022    1c                   ENDIF
027500021018      *
027600021018     c                   ENDSR
027700021018      *----------------------------------------------------------
027800021018      * Riempio la Ds
027900021018      *----------------------------------------------------------
028000021018     c     Ges_Ds        BEGSR
028100021022      *
028200021022      * se non trovata la tabella non valorizzo la ds
028300021022     c                   IF        O41err = 'E'
028400021022     c                   clear                   ds1H
028500021022     c                   movel     Ds1H          O41Dta
028600021022     c                   ELSE
028700021018      *
028800021021      * se ho trovato su file testi il tipo record H valorizzo
028900021021      * la Ds1H con i campi del file
029000050831     c                   IF        §1html = *blanks and DfntxtH <> *blanks
029100021018     c                   movel     §TxDes        §1HDes
029200021018     c                   movel     §TxPva        §1HPva
029300021018     c                   movel     §TxFts        §1HFts
029400021018     c                   movel     §TxSip        §1HFv1
029500021018     c                   movel     §TxSic        §1HFv2
029600021018     c                   movel     §TxScc        §1HFv3
029700021018     c                   movel     §TxSca        §1HFv4
029800021018     c                   movel     §TxStc        §1HFv5
029900021018     c                   movel     §TxSdc        §1HFv6
030000021018     c                   movel     §TxSpf        §1HFv7
030100021018     c                   movel     §TxSnp        §1HFv8
030200021018     c                   movel     §TxSri        §1HFv9
030300021022     c                   ENDIF
030400021021      *
030500021021      * passo la Ds
030600021018     c                   movel     Ds1H          O41Dta
030700021021     c                   move      Simfel        O41Pol
030800021022     c                   ENDIF
030900021018      *
031000021018     c                   ENDSR
031100021018      *----------------------------------------------------------
031200021018      * Sbr iniziale
031300021018      *----------------------------------------------------------
031400021018     c     INIZ          BEGSR
031500021018     c
031600021021     c                   clear                   o41pol
031700021021     c                   clear                   o41let
031800021021     c                   clear                   o41dta
031900021021     c                   clear                   o41err
032000021018      * Lingua
032100021018     c                   IF        I41Lin = *blanks
032200021018     c                   move      'I'           I41Lin
032300021018     c                   ENDIF
032400021018      *
032500021018     c                   ENDSR
032600010404      *---------------------------------------------------------------*
032700010404      * ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
032800010404      *---------------------------------------------------------------*
032900021021     c     *inzsr        BEGSR
033000010404      *
033100021021     c     *ENTRY        PLIST
033200021021     c                   PARM                    Fnlv41ds
033300020905      *
033400021021      * Inizzializzo variabili di work
033500021021     c                   clear                   Tibs34ds
033600021021     c                   clear                   Azuteds
033700021021     c                   clear                   DDatiUte
033800021021      *
033900021021     c     *dtaara       define    §azute        azuteds
034000021021     c     *dtaara       define    §datiute      ddatiute
034100020905      *
034200021021     c                   in(E)     *dtaara
034300021021     c                   IF        %Error
034400021021     c                   call      'TIBS34R'
034500021021     c                   parm                    Tibs34Ds
034600021021     c                   in        *dtaara
034700021021     c                   ENDIF
034800020905      *
034900021018      * Tabel
035000021021     c     kTab          klist
035100021022     c                   kfld                    kut
035200021022     c                   kfld                    Cod
035300021022     c                   kfld                    Key
035400021022      *
035500021021     c     kTab1         klist
035600021021     c                   kfld                    kut
035700021021     c                   kfld                    Cod
035800021021      *
035900021021     c     Ktab2         KLIST
036000021021     c                   kfld                    kut
036100021021     c                   kfld                    Cod2
036200021018      *
036300021018      * Testi
036400021021     c     kTxt          klist
036500021022     c                   kfld                    $let
036600021022     c                   kfld                    $Pol
036700021021     c                   kfld                    I41Lin
036800021022     c                   kfld                    $Pos
036900021018      *
037000021022     c     kTxt1         klist
037100021022     c                   kfld                    $let
037200021022     c                   kfld                    $Pol
037300021022     c                   kfld                    I41Lin
037400021022      *
037500021021     c                   endsr
