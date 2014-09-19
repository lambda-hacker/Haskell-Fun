-- Higher Order Functions 
module HOF where
       -- Map
       l_map f [] = []
       l_map f (x:xs) = f x : l_map f xs

       -- Map Pred
       l_map_p f p [] = []
       l_map_p f p (x:xs) | p x  = f x : l_map_p f p xs
       	       	      	  | otherwise = x : l_map_p f p xs

	
       -- Flat Map
       l_flatmap f [] = []
       l_flatmap f (x:xs) = f x ++ l_flatmap f xs

       -- Foldr
       l_foldr f v [] = v
       l_foldr f v (x:xs) = f x ( l_foldr f v xs )

       -- Foldl  -- #TODO
       -- foldl f v [] = v
       -- 

       -- Zip
       l_zip _ [] = []
       l_zip [] _ = []
       l_zip (x:xs) (y:ys) = (x, y) : l_zip xs ys

       -- ZipWith
       l_zipwith f _ [] = []
       l_zipwith f [] _ = []
       l_zipwith f (x:xs) (y:ys) = f x y : l_zipwith f xs ys

       -- Filter
       l_filter p [] = []
       l_filter p (x:xs) | p x = x : l_filter p xs
                     	 | otherwise = l_filter p xs
     
       -- Exist
       l_exist p [] = False
       l_exist p (x:xs) | p x = True
     	       	        | otherwise = l_exist p xs
  
       -- Take
       l_take n [] = []
       l_take 0 _  = []
       l_take n (x:xs) = x : l_take (n-1) xs

       -- Drop
       l_drop n [] = []
       l_drop 0 xx = xx
       l_drop n (x:xs) = l_drop (n-1) xs

       -- Reverse
       l_rev [] = []
       -- #TODO l_rev (x:xs) = l_rev xs : x : []

