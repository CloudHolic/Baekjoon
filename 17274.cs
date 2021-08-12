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
                var max = Math.Max(card.Front, card.Back);
                var min = Math.Min(card.Front, card.Back);
            
                var lastQuery = -1;
                for (var i = nm[1] - 1; i > -1; i--)
                {
                    if (queryList[i] >= max || queryList[i] < min)
                        continue;
                    
                    lastQuery = i;
                    break;
                }
            
                var largeQueryCount = 0;
                for (var i = lastQuery + 1; i < nm[1]; i++)
                    if (queryList[i] >= max)
                        largeQueryCount++;
            
                result += (lastQuery > -1) && (largeQueryCount % 2 == 0) ? max : min;
            });
            
            Console.WriteLine(result);
            return 0;
        }
    }
}