000100050629      *
000200050713      * ?Definizioni x gestire lo split del record passato
000300050629     D  Lun_dati       s              5i 0 INZ(0)
000400050629     D in_X_prima      s              5i 0 INZ(0)
000500050629     D  in_X           s              5i 0 INZ(0)
000600050629     D  da_X           s              5i 0 INZ(0)
000700050629     D Campo_Nr        s              3u 0 INZ(0)
000800050629     D Campo           s             40    INZ(' ')
000900050629     D Campo_Lungo     s              3u 0 INZ(0)
001000050629     D Campo_Vuoto     s              1    INZ(' ')
001100050629     D Campo_Numerico  s              1    INZ(' ')
001200050629     D Campo_Decimali  s              1    INZ(' ')
001300050629     D Num_Decimali    s              3u 0 INZ(0)
001400050629     D  X_Byte         s              5i 0 INZ(0)
001500050629     D    Byte         s              1    INZ(' ')
001600050713      *
001700050713      * ?Dati di Input  (Record ricevuto dal pgm chiamante e da splittare)
001800050713     D  Dati           s           1500
001900050713     D Decimal_Separ   s              1
002000050713     D Separa_campi    s              1
002100050610      *
002200050713      * ? Schiere di Output / Campi di Output
002300050713      * ? I dati da ripassare al chiamante devono essere :
002400050713      * ? 1 - il nr.dei campi splittati
002500050713      * ? 2 - una schiera contenente i campi splittati (lunghi max.40 chrs)
002600050713      * ? 3 - una schiera che definisce la lunghezza di ciascun campo
002700050713      * ? 4 - una schiera che dice se il campo è numerico o alfa
002800050713      * ? 5 - una schiera che dice se ci sono dei decimali (se numerico)
002900050629      *
003000050629     D    NR_campi     s              3u 0
003100050629
003200050629     D  Campi_Record   ds
003300050629     D    Dato_sk                    40    DIM(100)
003400050629
003500050629     D  Campi_Lunghi   ds
003600050629     D    LunDato_sk                  3u 0 DIM(100)
003700050629
003800050629     D Campi_Numerici  ds
003900050629     D    DatoNum_sk                  1    DIM(100)
004000050629
004100050629     D Campi_Decimali  ds
004200050629     D    Decimal_sk                  3u 0 DIM(100)
004300050713      *
004400050713      * contatore di schiere
004500050713     D  sk             s              5i 0 INZ(0)
004600050713      *
004700050713      * ?------------------------------------------------------------------ */
004800050629     D Digits          C                   CONST('0123456789')
004900050628      * ?------------------------------------------------------------------ */
005000050628     c     *entry        plist
005100050629      * ?     Input
005200050629     c                   parm                    Separa_campi
005300050629     c                   parm                    Decimal_Separ
005400050629     c                   parm                    dati
005500050629      * ?     Output
005600050629     c                   parm                    Nr_campi
005700050629     c                   parm                    Campi_Record
005800050629     c                   parm                    Campi_Lunghi
005900050629     c                   parm                    Campi_Numerici
006000050629     c                   parm                    Campi_Decimali
006100050713      **
006200050713      ** Pulizia DS da restituire
006300050713      **    (essendo una ridefinizione delle schiere --> pulizia schiere)
006400050629     c                   clear                   Nr_campi
006500050629     c                   clear                   Campi_Record
006600050629     c                   clear                   Campi_Lunghi
006700050629     c                   clear                   Campi_Numerici
006800050629     c                   clear                   Campi_Decimali
006900050629      **
007000050628      **  Controlla dove trovare il separatore
007100050713      **  e si deve partire la prima volta dalla prima posizione
007200050713      **  impostando la lunghezza massima della stringa di carattere passata
007300050713      **  che compone il record. (Questo per non perdere tempo dopo l'ultimo
007400050713      **  carattere valido con dei blanks inutili)
007500050629     c                   eval      in_X_prima = 1
007600050628     c                   eval      Lun_dati = %Len(%TrimR(Dati))
007700050628      **
007800050713      **  Esegue un loop byte x byte fino alla lunghezza massima
007900050713      **  per cercare dove si trova il separatore
008000050629     c                   do        Lun_dati      in_X
008100050628      **
008200050628      * ?   ( se trovato separatore imposta il nuovo campo )
008300050713      * ?     dal punto in cui si trova il primo byte non separatore fino al primo
008400050713      * ?     separatore trovato.
008500050629     c                   If        %Subst(Dati:in_X:1)=separa_campi
008600050628      **
008700050629      * ?   ( Che campo sta analizzando ?? --> il campo numero x della stringa )
008800050713      **      si memorizza il nr.campo che sta elaborando e resetta i flags
008900050628     c                   add       1             Campo_Nr
009000050629     c                   eval      Campo_Vuoto    = 'N'
009100050629     c                   eval      Campo_Numerico = 'N'
009200050629     c                   eval      Campo_Decimali = 'N'
009300050629     c                   eval      Num_Decimali   =  0
009400050628      **
009500050628      **  Imposta i campi
009600050713      **   occorre impostare da quale byte iniziare della stringa da splittare
009700050713      **   per poi eseguire la SUBSTRING e recepire l'informazione contenuta.
009800050629     c                   if        Campo_Nr = 1
009900050713      * ?  Se è il primo campo è il punto da cui partire quindi è il 1 byte
010000050629     c                   eval      da_X = 1
010100050629     c                   else
010200050713      * ?   punto da cui partire successivamente
010300050629     c                   eval      da_X = in_X_prima + 1
010400050629     c                   end
010500050629      **
010600050713      **  Lunghezza del contenuto è il punto in cui si è trovato il separatore
010700050713      **   meno il byte da dove inizia l'informazione.
010800050629     c                   eval      Campo_Lungo = in_X - da_X
010900050629      * ?   ( Campo Vuoto )
011000050713      **   se la differenza è zero allora il campo è vuoto ossia ci sono
011100050713      **   2 separatori contigui.
011200050629     c                   if        Campo_Lungo = 0
011300050713      * pulisce il contenuto del campo e memorizza che il campo non contiene
011400050713      * alcun valore.
011500050629     c                   eval      Campo_Vuoto = 'S'
011600050629     c                   clear                   campo
011700050629
011800050713      * Se invece è diverso da zero allora c'è qualcosa nel campo
011900050628     c                   else
012000050628      * ?   ( imposta il dato rilevato nel campo )
012100050713      *  quindi per SUBSTRING rileva l'informazione e la memorizza sul CAMPO generico
012200050629     c                   eval      campo = %subst(Dati:da_X:Campo_Lungo)
012300050629
012400050629      * ?   ( controlla se ha solo numeri )
012500050713      * E' utile sapere se sono contenuti solo numeri e se ci sono dei decimali
012600050713      *  oppure ci sono caratteri alfanumerici.
012700050629     c                   eval      Campo_Numerico = 'S'
012800050629     c                   z-add     0             Num_decimali
012900050629     c                   Do        Campo_Lungo   X_Byte
013000050629     c                   eval      Byte = %subst(Campo:X_Byte:1)
013100050629     c     digits        check     Byte
013200050629     c                   if        %Found
013300050713      ** se non è un numero, è il separatore decimale ??????
013400050629     c                   if        Byte = Decimal_Separ
013500050629     c                   eval      Campo_Decimali = 'S'
013600050629     c                   else
013700050713      ** se non lo è allora trattasi di campo alfanumerico.
013800050629     c                   eval      Campo_Numerico = 'N'
013900050629     c                   Leave
014000050629     c                   end
014100050629     c                   else
014200050713      **
014300050713      * se c'erano dei decimali quanti sono ???? Li conta
014400050713      ** tanti numeri podo il separatore decimale.
014500050629     c                   if        Campo_Decimali = 'S'
014600050629     c                   add       1             Num_decimali
014700050629     c                   end
014800050629     c                   end
014900050629     c                   endDo
015000050629      *
015100050628     c                   end
015200050628      **
015300050713      ** Le 5 definizioni di Output:
015400050629      ** Memorizza le informazioni di ciascun campo su schiere
015500050629     c                   z-add     Campo_Nr      Sk
015501061214     c                   if        Campo_Lungo > 40
015502061214     c                   z-add     40            LunDato_sk(Sk)
015503061214     c                   else
015600050629     c                   z-add     Campo_Lungo   LunDato_sk(Sk)
015601061214     c                   end
015700050629     c                   movel(p)  Campo         Dato_sk(Sk)
015800050629     c                   movel(p)  Campo_NumericoDatoNum_sk(Sk)
015900050629     c                   z-add     Num_Decimali  Decimal_sk(Sk)
016000050629      *
016100050713      ** Uscendo dal ciclo
016200050713      ** Memorizza la posizione del separatore per reiniziare un nuovo
016300050713      *   ciclo.
016400050629     c                   eval      in_X_prima = in_X
016500050629      **
016600050628     c                   EndIf
016700050629
016800050628     c                   EndDo
016900050609
017000050629      * ?   ( imposta campi di ritorno da schiere )
017100050629     c                   eval      Nr_campi = Sk
017200050629
017300030620     c                   seton                                        LR
017400050628      * ?------------------------------------------------------------------ */
