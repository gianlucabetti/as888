000100071128
000200110719      //********************************************************************************************
000300110719      //
000400110719      // Definizione file.
000500110719      //
000600110719      //********************************************************************************************
000700110719     Fwas7ifs0f UF A E             DISK
000800110719
000900071128      //********************************************************************************************
001000071128      //
001100071128      // Definizione variabili globali.
001200071128      //
001300071128      //********************************************************************************************
001400110721     D I_IdJob         S             26A
001500110721     D I_Ora           S              6A
001600110721     D I_FilILib       S             10A
001700110721     D I_FilINam       S             10A
001800110721     D I_FilIMbr       S             10A
001900110721     D I_FilFldr       S             30A
002000110721     D I_FilName       S            128A
002100110721     D I_FilExtn       S              5A
002200110721     D O_Esito         S              1S 0
002300071127
002400071128      //********************************************************************************************
002500071128      //
002600071128      // Dichiarazione parametri.
002700071128      //
002800071128      //********************************************************************************************
002900110721     C     *ENTRY        PLIST
003000110721     C                   PARM                    I_IdJob
003100110721     C                   PARM                    I_Ora
003200110721     C                   PARM                    I_FilILib
003300110721     C                   PARM                    I_FilINam
003400110721     C                   PARM                    I_FilIMbr
003500110721     C                   PARM                    I_FilFldr
003600110721     C                   PARM                    I_FilName
003700110721     C                   PARM                    I_FilExtn
003800110721     C                   PARM                    O_Esito
003900110721
004000071127      /FREE
004100110615        // Inizializzo esito a errore
004200110719        O_Esito = 1;
004300110620
004400110620
004500110719        // Verifica parametri d input
004600110719        if I_IdJob   = *blanks or
004700110719           I_Ora     = *blanks or
004800110721           I_FilILib = *blanks or
004900110721           I_FilINam = *blanks or
005000110719           I_FilFldr = *blanks or
005100110719           I_FilName = *blanks or
005200110719           I_FilExtn = *blanks;
005300110719        else;
005400110719
005500110721           // Verifica "particolarità"
005600110721           if %subst(%trim(I_FilFldr):%len(%trim(I_FilFldr)):1) = '/';
005700110721           else;
005800110721              I_FilFldr = %trim(I_FilFldr) + '/';
005900110721           endif;
006000110721
006100110719
006200110719           // Se tutto ok => genero il file nel IFS
006300110719
006400110719      /END-FREE
006500110719     C*
006600110719     C                   eval      wPATHIFS = '/BARTOLINI/TEMP/'
006700110719     C                   eval      wFILIFS  = I_FilName
006800110721     C                   eval      wEXTIFS  = I_FilExtn
006900110719     C*
007000110719     C* Pubblico il file nel IFS
007100110719     C                   call(e)   'TRBMF2IFC'
007200110721     C                   parm      I_FilILib     wLIBIN           10
007300110721     C                   parm      I_FilINam     wFILIN           10
007400110721     C                   parm      I_FilIMbr     wMBRIN           10
007500110719     C                   parm      '*'           wFLDSELIN        20
007600110719     C                   parm      *blanks       wNOMEALIAS       22
007700110719     C                   parm                    wPATHIFS        255
007800110719     C                   parm                    wFILIFS         255
007900110719     C                   parm                    wEXTIFS         255
008000110721     C                   parm      1024          wLUNGH            5 0
008100110719     C                   parm      'Y'           wOUTTRIM          1
008200110719     C                   parm      '*CRLF'       wENDLIN           6
008300110719     C                   parm      01252         wCCCSD            5 0
008400110719     C                   parm      *blanks       wMESSAGGIO       80
008500110719     C                   parm      '0'           wESITO            1
008600110719     C*
008700110719     C* Se a questo punto tutto ok => proseguo scivendo il log pilota
008800110719     C                   if        not %error and wESITO <> '2'
008900110719     C*
009000110719     C                   clear                   was7ifs0
009100110721     C                   eval      IFSINS    = (%dec(%date():*ISO)*1000000) +
009200110721     C                                         (%dec(I_Ora:6:0))
009300110719     C                   eval      IFSKEY    = %trim(I_IdJob) + %trim(I_Ora)
009400110719     C                   eval      IFSPTHS   = '/BARTOLINI/TEMP/'
009500110719     C                   eval      IFSFILS   = %trim(wFILIFS) + %trim(wEXTIFS)
009600110719     C                   eval      IFSPTHD   = '/QNTC/CEDINTRA/APPDATA/' +
009700110719     C                                         'POD/CLIENTI/' + %trim(I_FilFldr)
009800110721     C                   eval      IFSFILD   = IFSFILS
009900110721     C                   eval      IFSOPZ    = 'MOVE'
010000110719     C                   eval      IFSTRYTOT = 200
010100110719     C*
010200110719     C* Scarico il buffer d output
010300110719     C                   write(e)  was7ifs0
010400110719     C*
010500110719     C* Se qui tutto ok => allora transazione completata fully-correct
010600110719     C                   if        not %error
010700110719     C                   eval      O_Esito = 0
010800110719     C                   endif
010900110719     C*
011000110719     C                   endif
011100110719
011200110719
011300110719      /FREE
011400110719
011500110719        endif;
011600110719
011700110615
011800110719        *inlr = *on;
011900110719
012000071127
012100071127      /END-FREE
012200071129
