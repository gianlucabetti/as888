000100080611     H NOMAIN
000200081209     H bnddir('QC2LE')
000300071128     ***********************************************************************************************
000400071128     **
000500081209     ** Questo modulo verifica la presenza di un oggetto nell'IFS
000600081003     **
000700081003     ** Può essere usato solo come sottoprocedura.
000800081003     ** Ogni metodo restituisce un valore 10I:
000900081003     **             - = 0 => tutto ok
001000081003     **             - < 0 => errore
001100071128     **
001200081209     ** PARAMETRI DELLA SOTTOPROCEDURA: UBIFSCHKLK_Found...
001300071129     **
001400081003     ** Posizione Uso Tipo      Lunghezza Descrizione
001500081003     ** --------- --- --------- --------- ------------------------------
001600081209     ** 1         IN  CHARACTER 2048      Path IFS
001700081209     ** 2         IN  CHARACTER 255       File IFS (comprensivo di estensione)
001800081210     ** 3         IN  CHARACTER 3072      Full file name (path + file con estensione)
001900081210     ** 4         OUT CHARACTER 10        Results: *FOUND, *NOTFOUND, *INPUTERR
002000081003     ** --------- --- --------- --------- ------------------------------
002100071128     **
002200071128     **
002300071128     ** ESEMPI DI CHIAMATA
002400071128     **
002500081210     ** if UBIFSCHKLK_Found(pInPathIFS:pInFileIFS:pInFullIFS:pOutResult) = 0
002600071128     **
002700071128     ** ISTRUZIONI PER LA COMPILAZIONE
002800071128     **
002900081209     ** 1. Creare il modulo UBIFSCHKLK (Opz 15 PDM)
003000081209     ** 2. Creare/Aggiornare il programma di servizio UBIFSCHKLK (CRTSRVPGM / UPDSRVPGM)
003100080610     ** 4. Cancellare il modulo.
003200071128     **
003300071128     ***********************************************************************************************
003400071128
003500071128      //********************************************************************************************
003600071128      //
003700071128      // Definizione prototipi procedure.
003800071128      //
003900071128      //********************************************************************************************
004000081209     D/COPY GAITRASRC/SRCPROTOPR,UBIFSCHKLK
004100081209     D/COPY GAITRASRC/SRCPROTOPR,STAT
004200081209     D/COPY GAITRASRC/SRCPROTOPR,STAT_INFO
004300081209     D/COPY GAITRASRC/SRCPROTOPR,ERRNO
004400081210     D/COPY GAITRASRC/SRCPROTOPI,ERRNO
004500081210     D/COPY GAITRASRC/SRCCONST,ERRNO
004600071128
004700071128      //********************************************************************************************
004800071128      //
004900071128      // Definizione variabili globali.
005000071128      //
005100071128      //********************************************************************************************
005200081209     D PathFileName    S           3072A   INZ(*blanks)
005300081209     D RetVal          S             10i 0 INZ
005400071128
005500080611     P*--------------------------------------------------
005600081209     P* Procedure name: UBIFSCHKLK_Found...
005700081209     P* Purpose:        Verifica esistenza oggetto in IFS
005800080612     P* Returns:        Integer (Status_Code)
005900081209     P* Parameter:      pInPathIFS     => Percorso IFS
006000081209     P* Parameter:      pInFileIFS     => Nome file IFS (completo d estensione)
006100081210     P* Parameter:      pInFullIFS     => Full file name (path + file con estensione)
006200081209     P* Parameter:      pOutResult     => Risultato
006300080611     P*--------------------------------------------------
006400080611     P*
006500081209     P UBIFSCHKLK_Found...
006600071129     P                 B                   EXPORT
006700071129     D*
006800081209     D UBIFSCHKLK_Found...
006900080612     D                 PI            10I 0
007000081209     D   pInPathIFS                2048A
007100081209     D   pInFileIFS                 255A
007200081210     D   pInFullIFS                3072A
007300081209     D   pOutResult                  10A
007400081209
007500081209
007600080612
007700080612      //********************************************************************************************
007800080612      //
007900080612      // Definizione variabili work.
008000080612      //
008100080612      //********************************************************************************************
008200081003
008300081003
008400081003     C*
008500081003     C* Inizializzazioni
008600081209     C                   clear                   pOutResult
008700081209     C                   eval      pOutResult = '*NOTFOUND'
008800081008     C*
008900081008     C* Controllo parametri d input
009000081210     C                   if        pInFullIFS <> *blanks
009100081210     C                   eval      PathFileName = %trim(pInFullIFS) + x'00'
009200081210     C                   else
009300081210     C*
009400081209     C                   if        pInPathIFS = *blanks
009500081209     C                   eval      pOutResult = '*INPUTERR'
009600081209     C                   return    -1
009700081008     C                   endif
009800081209     C                   if        pInFileIFS = *blanks
009900081209     C                   eval      pOutResult = '*INPUTERR'
010000081209     C                   return    -1
010100081209     C                   endif
010200081209     C*
010300081210     C                   if        %subst(%trim(pInPathIFS):
010400081210     C                                    %len(%trim(pInPathIFS)):1) <> '/'
010500081209     C                   eval      pInPathIFS = %trim(pInPathIFS) + '/'
010600081209     C                   endif
010700081210     C                   if        %subst(%trim(pInPathIFS):1:1) <> '/'
010800081210     C                   eval      pInPathIFS = '/' + %trim(pInPathIFS)
010900081210     C                   endif
011000081210     C*
011100081210     C                   eval      PathFileName = %trim(pInPathIFS) +
011200081210     C                                            %trim(pInFileIFS) + x'00'
011300081210     C                   endif
011400081209     C*
011500081209     C* Reperisco gli attributi
011600081209     C                   eval      RetVal = stat(%addr(PathFileName)
011700081209     C                                          :%addr(stat_info))
011800081209     C                   if        RetVal < *zeros
011900081209     C                   eval      errnoPtr = getErrno()
012000081209     C                   if        errno = 3025
012100081209     C                   eval      pOutResult = '*NOTFOUND'
012200081209     C                   return    0
012300081209     C                   else
012400081209     C                   eval      pOutResult = 'ERR: ' + %char(errno)
012500081209     C                   return    errno * -1
012600081209     C                   endif
012700081209     C                   else
012800081209     C                   eval      pOutResult = '*FOUND'
012900081209     C                   return    0
013000081209     C                   endif
013100081209     C*
013200071129
013300081209     P UBIFSCHKLK_Found...
013400071129     P                 E
