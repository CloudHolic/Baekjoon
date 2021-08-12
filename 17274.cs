using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace CardFactory
{
    public static class Program
    {
        private record Card
        {
            public int Front { get; }
            public int Back { get; }

            public Card(IReadOnlyList<int> nums)
            {
                Front = nums[0];
                Back = nums[1];
            }
        }
        
        public static int Main()
        {
            var nm = Console.ReadLine().Split(' ').Select(int.Parse).ToArray();
            long result = 0;
            
            var cardList = new List<Card>(nm[0]);
            var queryList = new List<int>(nm[1]);

            for (var i = 0; i < nm[0]; i++)
            {
                var card = Console.ReadLine().Split(' ').Select(int.Parse).ToArray();
                cardList.Add(new Card(card));
            }

            for (var i = 0; i < nm[1]; i++)
            { 
                int k;
                var suc = int.TryParse(Console.ReadLine(), out k);
                queryList.Add(k);
            }

            Parallel.ForEach(cardList, card =>
            {
                var cur = card.Front > card.Back;
                var max = cur ? card.Front : card.Back;
                var min = cur ? card.Back : card.Front;
            
                for (int i = 0; i < nm[1]; i++)
                {
                    if (queryList[i] < min)
                        continue;
                    
                    if (queryList[i] >= max)
                        cur = !cur;
                    else if (!cur)
                        cur = true;
                }
            
                result += cur ? max : min;
            });
            
            Console.Write(result);
            return 0;
        }
    }
}