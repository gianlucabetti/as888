000100121224     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200090202      *-------------------------------------------------------------------------------
000300110202      * SUDDIVIDE in singoli campi una stringa contenente tutti gli indirizzi MAIL
000400110202      *  e restituisce max 10 indirizzi a cui spedire un messaggio.
000500090202      *-------------------------------------------------------------------------------
000600110621     d  esito          s              1
000700110202      *  Invio E-mail
000800110202     D    wrkEml       s            253
000900110202     D    wrkOgg       s             44
001000110202     D    wrkMsg       s           5000
001100110202      *
001200110202      * ?Definizioni x gestire lo split del record passato
001300110202     D  Lun_dati       s              5i 0 INZ(0)
001400110202     D in_X_prima      s              5i 0 INZ(0)
001500110202     D  Dati           s            253
001600110202     D separatore      s              1
001700110202     D Campo           s            253    INZ(' ')
001800110202      *
001900110202      *
002000110202     D  in_X           s              5i 0 INZ(0)
002100110202     D  da_X           s              5i 0 INZ(0)
002200110202     D Campo_Nr        s              3u 0 INZ(0)
002300110202     D Campo_Lungo     s              3u 0 INZ(0)
002400110202      *
002500050713      * ?------------------------------------------------------------------ */
002600050629     D Digits          C                   CONST('0123456789')
002700050628      * ?------------------------------------------------------------------ */
002800110202      * ?     Input
002900050628     c     *entry        plist
003000110208     C                   parm                    Indirizzi       253
003100110208     C                   parm                    Oggetto          44
003200110208     C                   parm                    Messaggio      5000
003300110208      *
003400110208     c                   z-add     0             Campo_Nr
003500110202      *
003600110202      **  Controlla dove trovare il separatore
003700110202      **  e si deve partire la prima volta dalla prima posizione
003800110202      **  impostando la lunghezza massima della stringa di carattere passata
003900110202      **  che compone il record. (Questo per non perdere tempo dopo l'ultimo
004000110202      **  carattere valido con dei blanks inutili)
004100110202      *
004200110202     c                   eval      in_X_prima = 1
004300110202     c                   eval      separatore = ';'
004400110202     c                   eval      Dati       = Indirizzi
004500110202     c                   eval      Lun_dati = %Len(%TrimR(Dati))
004600110202      *
004700110202      * esegue un controllo su l'ultimo carattere della stringa passata e se non c'è il
004800110202      *  separatore che ci si aspetta lo aggiunge per poter seguire lo split anche
004900110202      *  dell'ultimo campo.
005000110202     c                   if        %subst(Dati:Lun_dati:1) <> separatore
005100110202     c                   eval      Lun_dati = Lun_dati + 1
005200110202     c                   eval      %subst(Dati:Lun_dati:1) = separatore
005300110202     c                   end
005400110202      **
005500110202      **  Esegue un loop byte x byte fino alla lunghezza massima
005600110202      **  per cercare dove si trova il separatore
005700110202     c                   do        Lun_dati      in_X
005800110202      **
005900110202      * ?   ( se trovato separatore imposta il nuovo campo )
006000110202      * ?     dal punto in cui si trova il primo byte non separatore fino al primo
006100110202      * ?     separatore trovato.
006200110202     c                   If        %Subst(Dati:in_X:1) = separatore
006300110202      **
006400110202      **      si memorizza il nr.campo che sta elaborando e resetta i flags
006500110202     c                   add       1             Campo_Nr
006600110202      **
006700110202      **  Imposta i campi
006800110202      **   occorre impostare da quale byte iniziare della stringa da splittare
006900110202      **   per poi eseguire la SUBSTRING e recepire l'informazione contenuta.
007000110202     c                   if        Campo_Nr = 1
007100110202      * ?  Se è il primo campo è il punto da cui partire quindi è il 1 byte
007200110202     c                   eval      da_X = 1
007300110202     c                   else
007400110202      * ?   punto da cui partire successivamente
007500110202     c                   eval      da_X = in_X_prima + 1
007600110202     c                   end
007700110202      **
007800110202      **  Lunghezza del contenuto è il punto in cui si è trovato il separatore
007900110202      **   meno il byte da dove inizia l'informazione.
008000110202     c                   eval      Campo_Lungo = in_X - da_X
008100110202      * ?   ( Campo Vuoto )
008200110202      **   se la differenza è zero allora il campo è vuoto ossia ci sono
008300110202      **   2 separatori contigui.
008400110202     c                   if        Campo_Lungo > 0
008500110202      * ?   ( imposta il dato rilevato nel campo )
008600110202      *  quindi per SUBSTRING rileva l'informazione e la memorizza sul CAMPO generico
008700110202     c                   eval      campo = %subst(Dati:da_X:Campo_Lungo)
008800110202      *
008900110202     c                   exsr      INVIA_MAIL
009000110202     c                   end
009100110202      **
009200110202      ** Uscendo dal ciclo
009300110202      ** Memorizza la posizione del separatore per reiniziare un nuovo
009400110202      *   ciclo.
009500110202     c                   eval      in_X_prima = in_X
009600110202     c                   EndIf
009700110202
009800110202     c                   EndDo
009900110202
010000110202     c                   seton                                        RT
010100110202      *--------------------------------------------------------------------
010200110202      *   INVIA MAIL
010300110202      *____________________________________________________________________
010400110202     c     INVIA_MAIL    BEGSR
010500110202      *
010600110202     C* Inizializzo variabili
010700110202     C                   eval      wrkEml = Campo
010800110202     C                   eval      wrkOgg = Oggetto
010900110202     C                   eval      wrkMsg = Messaggio
011000110202      *
011100110202     C                   call(e)   'TIS701C'
011200110202     C                   parm                    wrkEml
011300110202     C                   parm                    wrkOgg
011400110202     C                   parm                    wrkMsg
011500110202      *
011600110202     c                   ENDSR
011700050628      * ?------------------------------------------------------------------ */
011800110621      * ?------------------------------------------------------------------ */
011900110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
012000110621      * ?------------------------------------------------------------------ */
012100110621     C     *pssr         BEGSR
012200110621     C
012300110621     C                   eval      esito = 'E'
012400110621     C                   ENDSR     '*CANCL'
012500110621     C
012600110621      *  ------------------------------------------------------------------ */
