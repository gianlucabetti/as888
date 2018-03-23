000100080611     H NOMAIN
000200071128     ***********************************************************************************************
000300071128     **
000400080612     ** Questo modulo reperisce il NETA del sistema AS/400 corrente.
000600071128     **
000700080611     ** Può essere usato solo come sottoprocedura.
000800080612     ** Ogni metodo restituisce un valore 10I relativo all'esito della propria esecuzione.
000900071128     **
001000080612     ** PARAMETRI DELLA SOTTOPROCEDURA: UBRTVNETA_Rtv
001800071129     **
001900071129     ** Posizione Uso Tipo      Descrizione
002000071129     ** --------- --- --------- ------------------------------
002100080612     ** 1         OUT CHARACTER NETA sistema AS/400 corrente
002600071129     ** --------- --- --------- ------------------------------
002700071128     **
002800071128     **
002900071128     ** ESEMPI DI CHIAMATA
003000071128     **
003100080616     ** if UBRTVNETA_Rtv(pOutSysNeta) = 0
003200071128     **
003300071128     ** ISTRUZIONI PER LA COMPILAZIONE
003400071128     **
003500080612     ** 1. Creare il modulo UBRTVNETA (Opz 15 PDM)
003600080612     ** 2. Creare/Aggiornare il programma di servizio UBRTVNETA (CRTSRVPGM / UPDSRVPGM)
003700080610     ** 4. Cancellare il modulo.
003800071128     **
003900071128     ***********************************************************************************************
004000071128
004100071128      //********************************************************************************************
004200071128      //
004300071128      // Definizione prototipi procedure.
004400071128      //
004500071128      //********************************************************************************************
004600080612     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
004700071128
004800071128      //********************************************************************************************
004900071128      //
005000071128      // Definizione variabili globali.
005100071128      //
005200071128      //********************************************************************************************
005300080612     D
006700071128
006800080611     P*--------------------------------------------------
006900080612     P* Procedure name: UBRTVNETA_Rtv
007000080612     P* Purpose:        Reperimento NETA sistema AS/400 corrente
007100080612     P* Returns:        Integer (Status_Code)
007101080612     P* Parameter:      pOutSysNeta   => NETA sistema AS/400 corrente
007200080611     P*--------------------------------------------------
012900080611     P*
013000080612     P UBRTVNETA_Rtv...
013100071129     P                 B                   EXPORT
013200071129     D*
013300080612     D UBRTVNETA_Rtv...
013400080612     D                 PI            10I 0
013500080612     D   pOutSysNeta                  8A
013501080612
013504080612
013505080612      //********************************************************************************************
013506080612      //
013507080612      // Definizione variabili work.
013508080612      //
013509080612      //********************************************************************************************
013512080612     D QWCRNETA        PR                  ExtPgm('QWCRNETA')
013513080612     D   RcvVar                   65535A   OPTIONS(*VARSIZE)
013514080612     D   RcvVarLen                   10I 0 CONST
013515080612     D   NbrNetAtr                   10I 0 CONST
013516080612     D   AttrNames                65535A   OPTIONS(*VARSIZE) CONST
013517080612     D   ErrorCode                  256A
013519080612
013520080612     D* Error code structure
013521080612     D EC              DS
013522080612     D*                                    Bytes Provided (size of struct)
013523080612     D  EC_BytesP              1      4B 0 INZ(256)
013524080612     D*                                    Bytes Available (returned by API)
013525080612     D  EC_BytesA              5      8B 0 INZ(0)
013526080612     D*                                    Msg ID of Error Msg Returned
013527080612     D  EC_MsgID               9     15
013528080612     D*                                    Reserved
013529080612     D  EC_Reserve            16     16
013530080612     D*                                    Msg Data of Error Msg Returned
013531080612     D  EC_MsgDta             17    256
013532080612
013533080612     D* Receiver variable for QWCRNETA with only one attribute
013534080612     D RV              ds
013535080612     D*                                    Number of Attrs returned
013536080612     D   RV_Attrs                    10I 0
013537080612     D*                                    Offset to first attribute
013538080612     D   RV_Offset                   10I 0
013539080612     D*                                    Add'l data returned.
013540080612     D   RV_Data                      1A   DIM(1000)
013541080612
013542080612     D* Network attribute structure
013543080612     D p_NA            S               *
013544080612     D NA              ds                  based(p_NA)
013545080612     D*                                    Attribute Name
013546080612     D   NA_Attr                     10A
013547080612     D*                                    Type of Data.  C=Char, B=Binary
013548080612     D   NA_Type                      1A
013549080612     D*                                    Status. L=Locked, Blank=Normal
013550080612     D   NA_Status                    1A
013551080612     D*                                    Length of Data
013552080612     D   NA_Length                   10I 0
013553080612     D*                                    Actual Data (in character)
013554080612     D   NA_DataChr                1000A
013555080612     D*                                    Actual Data (in binary)
013556080612     D   NA_DataInt                  10I 0 overlay(NA_DataChr:1)
013557080612
013558080612     C* Call API to get system name
013559080612     C*   -1 = API returned an error
013560080612     C                   callp     QWCRNETA(RV: %size(RV): 1: 'SYSNAME': EC)
013561080612     C                   if        EC_BytesA > 0
013562080612     C                   return    -1
013563080612     C                   endif
013564080612
013565080612     C*   -2 = RcvVar contained data that we
013566080612     C*        dont understand :(
013567080612     C                   if        RV_Attrs <> 1
013568080612     C                               or RV_Offset < 8
013569080612     C                               or RV_Offset > 1000
013570080612     C                   return    -2
013571080612     C                   endif
013572080612
013573080612     C*   Attach NetAttr structure
013574080612     C                   eval      RV_Offset = RV_Offset - 7
013575080612     C                   eval      p_NA = %addr(RV_Data(RV_Offset))
013576080612
013577080612     C*   -3 = NetAttr structure had data
013578080612     C*        that we don't understand :(
013579080612     C                   if        NA_Attr <> 'SYSNAME'
013580080612     C                               or NA_Length < 1
013581080612     C                               or NA_Length > 8
013582080612     C                   return    -3
013583080612     C                   endif
013584080612
013585080612     C*   -4 = Network attributes are locked
013586080612     C                   if        NA_Status = 'L'
013587080612     C                   return    -4
013588080612     C                   endif
013589080612
013590080612     C*   Success
013591080612     C                   eval      pOutSysNeta = %subst(NA_DataChr:1:NA_Length)
013592080612     C                   return    0
028900071129
029000080612     P UBRTVNETA_Rtv...
029100071129     P                 E
