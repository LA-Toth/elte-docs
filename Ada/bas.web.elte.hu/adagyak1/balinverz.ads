generic
   type Felcsoport is ( <> );

   Egyseg : in Felcsoport;

   with function "*" ( A, B : Felcsoport ) return Felcsoport is <>;

procedure Balinverz ( A : in Felcsoport; B : out Felcsoport;
                      Van : out Boolean );